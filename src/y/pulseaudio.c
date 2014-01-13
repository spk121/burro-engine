/* This is audio engine's backend for using PulseAudio.
   Any pulseaudio specific calls go here. */
#include <pulse/pulseaudio.h>
#include <glib.h>
#include <string.h>
#include <math.h>
#include "x/xpulseaudio.h"
#include "y/rand.h"
#include "y/loop.h"
#include "pulseaudio.h"
#include "engine.h"

#define BURRO_PROP_MEDIA_ROLE "game"
#define BURRO_PROP_APPLICATION_ID "com.lonelycactus.projectburro"
#define BURRO_PROP_APPLICATION_NAME "ProjectBurro"

#define CHANNELS_COUNT 16

int16_t audio_buf[CHANNEL_COUNT][AUDIO_BUFFER_SIZE];
double audio_time;

typedef struct pulse_priv_tag {
    pa_context_state_t state;
    pa_mainloop *loop;
    //pa_threaded_mainloop *loop;
    gboolean finalize;
    int samples_written;
} pulse_priv_t;
static pulse_priv_t pulse;

static void cb_audio_context_state(pa_context *c, void *userdata);
static void cb_audio_stream_started(pa_stream *p,
                                    void *userdata);
static void cb_audio_stream_write(pa_stream *p, size_t nbytes, void *userdata);

/* This callback gets called when our context changes state.  We
 * really only care about when it's ready or if it has failed. */
static void cb_audio_context_state(pa_context *c, void *userdata)
{
    pulse.state = (int) xpa_context_get_state(c);
    switch(pulse.state)
    {
    case PA_CONTEXT_UNCONNECTED:
        g_debug("PulseAudio set context state to UNCONNECTED");
        break;
    case PA_CONTEXT_CONNECTING:
        g_debug("PulseAudio set context state to CONNECTING");
        break;
    case PA_CONTEXT_AUTHORIZING: 
        g_debug("PulseAudio set context state to AUTHORIZING");
        break;
    case PA_CONTEXT_SETTING_NAME:
        g_debug("PulseAudio set context state to SETTING NAME");
        break;
    case PA_CONTEXT_FAILED:
        g_debug("PulseAudio set context state to FAILED");
        break;
    case PA_CONTEXT_TERMINATED:
        g_debug("PulseAudio set context state to TERMINATED");
        break;
    case PA_CONTEXT_READY:
        g_debug("PulseAudio set context state to READY");
        break;
    default:
        g_debug("PulseAudio set context state to %d", pulse.state);
        break;
    }
}

/* This callback is called when the server starts playback after an
 * underrun or on initial startup. */
static void cb_audio_stream_started(pa_stream *p, void *userdata)
{
    g_debug("PulseAudio started playback");
}
    
/* This is called when new data may be written to the stream.  If we
  have data, we can ship it, otherwise we just note that the stream is
  waiting.  */
static void cb_audio_stream_write(pa_stream *p, size_t nbytes, void *userdata)
{
    int n = nbytes;
    uint16_t *buf;
    int i;
    
    g_debug("Pulseaudio requests %d bytes", nbytes);
    n = nbytes / 2;
    buf = g_new(uint16_t, n);
    for(i = 0; i < n; i ++)
    {
        buf[i] = 0;
        for (int j = 0; j < CHANNEL_COUNT; j ++)
            buf[i] += audio_buf[j][i];
    }
    xpa_stream_write(p, buf, nbytes);
    g_free(buf);

    for (int j = 0; j < CHANNEL_COUNT; j ++)
        g_memmove(&(audio_buf[j][0]), &(audio_buf[j][n]),  sizeof(int16_t) * (AUDIO_BUFFER_SIZE - n));
    audio_time = loop_time();
    pulse.samples_written += n;
}

void pulse_initialize_audio()
{
    pa_mainloop_api *vtable;
    pa_proplist *main_proplist;
    pa_proplist *stream_proplist;
    pa_context *context;
    pa_sample_spec sample_specification;
    pa_channel_map channel_map;
    pa_buffer_attr buffer_attributes;
    pa_stream *stream;
    int ret = 1;



    /* PROPLIST: Only the PA_PROP_MEDIA_ROLE is important.  */
    main_proplist = xpa_proplist_new();
    xpa_proplist_sets(main_proplist, PA_PROP_MEDIA_ROLE, BURRO_PROP_MEDIA_ROLE);
    xpa_proplist_sets(main_proplist, PA_PROP_APPLICATION_ID, BURRO_PROP_APPLICATION_ID);
    xpa_proplist_sets(main_proplist, PA_PROP_APPLICATION_NAME, BURRO_PROP_APPLICATION_NAME);

    /* SAMPLE_SPEC:  we describe the data going into our channel */
    sample_specification.format = PA_SAMPLE_S16NE;
    sample_specification.rate = AUDIO_SAMPLE_RATE_IN_HZ;
    sample_specification.channels = 1;

    g_assert(pa_sample_spec_valid(&sample_specification));

    /* CHANNEL_MAP: Then we say which speakers we're using.  The game is mono, so
       that makes it simple. */
    pa_channel_map_init_extend(&channel_map, 1, PA_CHANNEL_MAP_DEFAULT);

    if (!pa_channel_map_compatible(&channel_map, &sample_specification))
        g_error("Channel map doesn't match sample specification");

    {
        char tss[100], tcm[100];
        g_debug("Opening a stream with sample specification '%s' and channel map '%s'.",
                pa_sample_spec_snprint(tss, sizeof(tss), &sample_specification),
                pa_channel_map_snprint(tcm, sizeof(tcm), &channel_map));
    }


    memset(&pulse, 0, sizeof(pulse));

    /* Allocate a new mainloop object to handle the audio polling */
    pulse.loop = xpa_mainloop_new();
    //pulse.loop = pa_threaded_mainloop_new();

    /* For some stupid reason, we can't use the mainloop structure
       directly to make a context.  We instead grab its vtable. */
    vtable = xpa_mainloop_get_api(pulse.loop);
    // vtable = pa_threaded_mainloop_get_api(pulse.loop);

    
    /* A context is the basic object for a connection to a PulseAudio
       server. It multiplexes commands, data streams and events
       through a single channel.
    */
    context = xpa_context_new_with_proplist(vtable,
                                            BURRO_PROP_APPLICATION_NAME,
                                            main_proplist);
    xpa_proplist_free (main_proplist);

    /* A context must be connected to a server before any operation
     * can be issued. */
    pulse.state = PA_CONTEXT_UNCONNECTED;
    xpa_context_set_state_callback(context,
                                   cb_audio_context_state, NULL);

    /* Connect the context */
    xpa_context_connect_to_default_server(context);

    /* We wait (blocking) for the daemon to reply that the connection
     * is good. The value e.priv.audio_state is set in
     * cb_audio_context_state().  */
    while(TRUE)
    { 
        xpa_mainloop_blocking_iterate (pulse.loop);
        // pa_threaded_mainloop_blocking_iterate (pulse.loop);
        if (pulse.state == PA_CONTEXT_FAILED)
            g_error("Failed to ready the connection to the PulseAudio daemon");
        else if  (pulse.state == PA_CONTEXT_TERMINATED)
            g_error("Premature termination of the connection to the PulseAudio daemon");
        else if (pulse.state == PA_CONTEXT_READY)
        {
            g_debug("Connection to PulseAudio daemon is ready");
            break;
        }
    }
    /* Now we need to add our mono audio channel to the connection */

    /* BUFFER_ATTRIBUTES: Here we set the buffering behavior of the
     * audio.  We want low latency.

       One wiki suggests that to set a specific latency, 
       1. use pa_usec_to_bytes(&ss, ...) to convert the latency from a time unit to bytes
       2. use the PA_STREAM_ADJUST_LATENCY flag
       3. set pa_buffer_attr::tlength to latency in samples
       4. set rest of pa_buffer_attr to -1

       http://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/LatencyControl
    */
    
    buffer_attributes.tlength = pa_usec_to_bytes (AUDIO_LATENCY_REQUESTED_IN_MILLISECONDS * MICROSECONDS_PER_MILLISECOND,
                                                  &sample_specification);
    buffer_attributes.maxlength = -1; /* -1 == default */
    buffer_attributes.prebuf = -1;
    buffer_attributes.minreq = -1;
    buffer_attributes.fragsize = -1;

    /* PROPERTY_LIST: Then we set up a structure to hold PulseAudio
     * properties for this. */
    stream_proplist = xpa_proplist_new();
    xpa_proplist_sets(stream_proplist, PA_PROP_MEDIA_NAME, "mono channel");
    xpa_proplist_sets(stream_proplist, PA_PROP_MEDIA_ROLE, "game");

    /* STREAM: Group everything together as a stream */
    stream = xpa_stream_new_with_proplist(context,
                                          "mono channel", /* Stream name */
                                          &sample_specification,
                                          &channel_map,
                                          stream_proplist); 
    xpa_proplist_free (stream_proplist);

    xpa_stream_set_started_callback(stream, cb_audio_stream_started, NULL);
    xpa_stream_set_write_callback(stream, cb_audio_stream_write, NULL);
    
    /* Connect the stream to the audio loop */
    xpa_stream_connect_playback_to_default_device (stream,
                                                   &buffer_attributes,
                                                   PA_STREAM_ADJUST_LATENCY);
    /* Finally done! */
    audio_time = loop_time();
    g_debug("PulseAudio initialization complete");
}

/* This finalizer is called if we are shutting down
   cleanly */
void pulse_finalize_audio()
{
    // xpa_mainloop_free(pulse.loop);
    pa_threaded_mainloop_free(pulse.loop);
    pulse.finalize = TRUE;
    g_debug("PulseAudio finalization complete");
}

void pulse_update_audio()
{
    pulse.samples_written = 0;
    while (xpa_mainloop_nonblocking_iterate(pulse.loop) > 0)
        ;
}

void pulse_mainloop()
{
    // pa_threaded_mainloop_start(pulse.loop);
}

static void generate_tone_data(double D_attack, double D_decay, double D_sustain, double D_release,
                               double F_initial, double F_attack, double F_sustain, double F_release,
                               double A_attack, double A_sustain,
                               double duty, _Bool noise, int waveform,
                               int16_t **buffer, size_t *length)
{
    /* D = duration in sec
       F = frequency in Hz
       A = amplitude, from 0.0 to 1.0 */
	double t, t_start, amplitude, frequency, period;
    double duration;
    size_t i;
    int first;
    int level_a, level_b;

    *buffer = NULL;
    *length = 0;

    g_return_if_fail (F_initial >= 12.0 && F_initial < 22050);
    g_return_if_fail (D_attack >= 0.0);
    
    duration = D_attack + D_decay + D_sustain + D_release;
    *length = ceil(duration * (double) AUDIO_SAMPLE_RATE_IN_HZ);
    *buffer = g_new(int16_t, *length);

    t = 0.0;
    t_start = 0.0;
    i = 0;
    period = 0.0;
    first = TRUE;
    while (i < *length)
    {
        if(first || t - t_start >= period)
        {
            if (first)
                first = FALSE;
            else
                while (t - t_start >= period)
                    t_start += period;

            if (t < D_attack)
            {
                amplitude = (A_attack / D_attack) * t;
                frequency = ((F_attack - F_initial) / D_attack) *  t + F_initial;
            }
            else if (t < D_attack + D_decay)
            {
                amplitude = ((A_sustain - A_attack) / D_decay) * (t - D_attack) + A_attack;
                frequency = ((F_sustain - F_attack) / D_decay) * (t - D_attack) + F_attack;
            }
            else if (t < D_attack + D_decay + D_sustain)
            {
                amplitude = A_sustain;
                frequency = F_sustain;
            }
            else if (t < D_attack + D_decay + D_sustain + D_release)
            {
                amplitude = (-A_sustain / D_release) * (t - D_attack - D_decay - D_sustain) + A_sustain;
                frequency = ((F_release - F_sustain) / D_release) * (t - D_attack - D_decay - D_sustain) + F_sustain;
            }
            else
            {
                amplitude = 0;
                frequency = F_release;
            }
            period = 1.0 / frequency;
            if (noise)
            {
                if(rand_int_range (0, 2))
                    level_a = 4095 * amplitude;
                else
                    level_a = -4095 * amplitude;
                if(rand_int_range (0, 2))
                    level_b = 4095 * amplitude;
                else
                    level_b = -4095 * amplitude;
            }
            else
            {
                level_a = 4095 * amplitude;
                level_b = -4095 * amplitude;
            }

        }
        if (t - t_start < period * duty)
        {
            if(waveform == 0)
                (*buffer)[i] = level_a;
            else if (waveform == 1)
                (*buffer)[i] = level_a * sin(M_PI * (t - t_start) / (period * duty));
        }
        else if (t - t_start < period)
        {
            if(waveform == 0)
                (*buffer)[i] = level_b;
            else if (waveform == 1)
                (*buffer)[i] = level_b * sin(M_PI * ((t - t_start) - period * duty) / (period * (1.0 - duty)));
        }
        i ++;
        t += 1.0 / (double) AUDIO_SAMPLE_RATE_IN_HZ;
    }
#if 1
    {
        FILE *fp;
        if(noise)
            fp = fopen("noise.txt", "wt");
        else
            fp = fopen("wave.txt", "wt");
        for(size_t i2 = 0; i2 < *length; i2++)
            fprintf(fp, "%d %d\n", i2, (int)(*buffer)[i2]);
        fclose(fp);
    }
#endif    
}


void tone(int channel, double start_time,
          double D_attack, double D_decay, double D_sustain, double D_release,
          double F_initial, double F_attack, double F_sustain, double F_release,
          double A_attack, double A_sustain,
          double duty, int noise, int waveform)
{
    int16_t *buffer;
    size_t length;
    double now = loop_time();
    double time_since_last_update = now - audio_time;
    double delta_t;
    int delta_i, i2, i;

    if (start_time == 0.0)
    {
        delta_t = 0.0;
        g_debug("Generate %f Hz tone on channel %d to start in %f s (%f sec since las update)",
                F_initial, channel, delta_t, time_since_last_update);
    }
    else
    {
        delta_t = start_time - audio_time;
        g_debug("Generate %f Hz tone on channel %d to start in %f s (%f sec since las update)",
                F_initial, channel, delta_t, time_since_last_update);
    }
        
    generate_tone_data(D_attack, D_decay, D_sustain, D_release,
                       F_initial, F_attack, F_sustain, F_release,
                       A_attack, A_sustain,
                       duty, noise, waveform,
                       &buffer, &length);

    delta_i = delta_t * AUDIO_SAMPLE_RATE_IN_HZ;
    for(i = 0; i < length; i++)
    {
        i2 = i + delta_i;
        if (i2 >= 0 && i2 < AUDIO_BUFFER_SIZE)
            audio_buf[channel][i2] = buffer[i];
    }
    g_free(buffer);
}



/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
