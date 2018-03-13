/* This is audio engine's backend for using PulseAudio.
   Any pulseaudio specific calls go here. */
#include <string.h>
#include <math.h>
#include "../x.h"
#include "audio_model.h"
#include "rand.h"
#include "loop.h"
#include "pulseaudio.h"
#include "eng.h"

#define USE_GLIB_MAINLOOP

#ifdef USE_GLIB_MAINLOOP
#include <pulse/glib-mainloop.h>
#endif

#define BURRO_PROP_MEDIA_ROLE "game"
#define BURRO_PROP_APPLICATION_ID "com.lonelycactus.projectburro"
#define BURRO_PROP_APPLICATION_NAME "ProjectBurro"

typedef struct pulse_priv_tag {
    pa_context_state_t state;
    pa_context *context;
#ifdef USE_GLIB_MAINLOOP
    pa_glib_mainloop *loop;
#else
    pa_mainloop *loop;
    //pa_threaded_mainloop *loop;
#endif
    gboolean finalize;
    unsigned samples_written;
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
        pulse_initialize_audio_step_2 (c);
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
    size_t n = nbytes / sizeof(int16_t);

    g_return_if_fail (p != NULL);

    pa_usec_t usec = xpa_stream_get_time (p);
    if (usec != 0)
        g_debug("Pulseaudio time is %"PRIu64", requests %zu samples", usec, n);
    if (n > AUDIO_BUFFER_SIZE)
    {
        g_warning ("Pulseaudio buffer read overflow %zu > %u",
                   n, AUDIO_BUFFER_SIZE);
        n = AUDIO_BUFFER_SIZE;
        nbytes = n * sizeof (uint16_t);
    }
    const int16_t *wav = audio_model_get_wave(1.0e-6 * usec, n);
    xpa_stream_write(p, wav, nbytes);
    pulse.samples_written += n;
}

void pulse_initialize_audio_step_1()
{
    pa_mainloop_api *vtable = NULL;
    pa_proplist *main_proplist = NULL;

    pulse.loop = xpa_glib_mainloop_new (g_main_context_default());
#ifdef USE_GLIB_MAINLOOP
    vtable = pa_glib_mainloop_get_api (pulse.loop);
#else
    vtable = xpa_mainloop_get_api(pulse.loop);
#endif

    /* PROPLIST: Only the PA_PROP_MEDIA_ROLE is important.  */
    main_proplist = xpa_proplist_new();
    xpa_proplist_sets(main_proplist, PA_PROP_MEDIA_ROLE, BURRO_PROP_MEDIA_ROLE);
    xpa_proplist_sets(main_proplist, PA_PROP_APPLICATION_ID,
                      BURRO_PROP_APPLICATION_ID);
    xpa_proplist_sets(main_proplist, PA_PROP_APPLICATION_NAME,
                      BURRO_PROP_APPLICATION_NAME);

    /* A context is the basic object for a connection to a PulseAudio
       server. It multiplexes commands, data streams and events
       through a single channel.  */
    pulse.context = xpa_context_new_with_proplist(vtable,
                                                  BURRO_PROP_APPLICATION_NAME,
                                                  main_proplist);
    xpa_context_set_state_callback(pulse.context,
                                   cb_audio_context_state, NULL);

    pulse.state = PA_CONTEXT_UNCONNECTED;

    /* Connect the context */
    xpa_context_connect_to_default_server(pulse.context);

}

void pulse_initialize_audio_step_2(pa_context *context)
{
    pa_proplist *stream_proplist;
    pa_sample_spec sample_specification;
    pa_channel_map channel_map;
    pa_buffer_attr buffer_attributes;
    pa_stream *stream;

    /* Now we need to add our mono audio channel to the connection */

    /* SAMPLE_SPEC:  we describe the data going into our channel */
    sample_specification.format = PA_SAMPLE_S16NE;
    sample_specification.rate = AUDIO_SAMPLE_RATE_IN_HZ;
    sample_specification.channels = 1;
    g_assert(pa_sample_spec_valid(&sample_specification));

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

    /* CHANNEL_MAP: Then we say which speakers we're using.  The game
       is mono, so - that makes it simple. */

    pa_channel_map_init_extend(&channel_map, 1, PA_CHANNEL_MAP_DEFAULT);

    if (!pa_channel_map_compatible(&channel_map, &sample_specification))
        g_error("Channel map doesn't match sample specification");

    {
        char tss[100], tcm[100];
        g_debug("Opening a stream with sample specification '%s' and channel map '%s'.",
                pa_sample_spec_snprint(tss, sizeof(tss), &sample_specification),
                pa_channel_map_snprint(tcm, sizeof(tcm), &channel_map));
    }

    /* STREAM: Group everything together as a stream */
    g_assert (pulse.context == context);
    stream = xpa_stream_new_with_proplist(pulse.context,
                                          "mono channel", /* Stream name */
                                          &sample_specification,
                                          &channel_map,
                                          stream_proplist);
    xpa_proplist_free (stream_proplist);

    xpa_stream_set_started_callback(stream, cb_audio_stream_started, NULL);
    xpa_stream_set_write_callback(stream, cb_audio_stream_write, NULL);

    pulse.samples_written = 0U;

    /* Connect the stream to the audio loop */
    xpa_stream_connect_playback_to_default_device (stream, context,
                                                   &buffer_attributes,
                                                   /* PA_STREAM_ADJUST_LATENCY | */
                                                   PA_STREAM_INTERPOLATE_TIMING | PA_STREAM_AUTO_TIMING_UPDATE);
    /* Finally done! */
    g_debug("PulseAudio initialization complete");
}

/* This finalizer is called if we are shutting down cleanly */
void pulse_finalize_audio()
{
    // xpa_mainloop_free(pulse.loop);
#ifdef USE_GLIB_MAINLOOP
    pa_glib_mainloop_free (pulse.loop);
#else
    pa_threaded_mainloop_free(pulse.loop);
#endif
    pulse.finalize = TRUE;
    g_debug("PulseAudio finalization complete");
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
