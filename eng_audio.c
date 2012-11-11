#include <math.h>
#include <pulse/pulseaudio.h>
#include "engine.h"
#include "eng_audio.h"

#define BURRO_PROP_MEDIA_ROLE "game"
#define BURRO_PROP_APPLICATION_ID "com.lonelycactus.projectburro"
#define BURRO_PROP_APPLICATION_NAME "ProjectBurro"

static void cb_audio_context_state(pa_context *c, void *userdata);
void cb_audio_stream_started(pa_stream *p,
                                 void *userdata);
void cb_audio_stream_write(pa_stream *p, size_t nbytes, void *userdata);
static void generate_tone_data(double D_attack, double D_decay, double D_sustain, double D_release,
                               double F_initial, double F_attack, double F_sustain, double F_release,
                               double A_attack, double A_sustain,
                               double duty, _Bool noise,
                               uint8_t **buffer, size_t *length);
void initialize_channel(int i);
static void start_noise(int i);
static void start_tone(int i);
static void stop_noise(int i);
static void stop_tone(int i);
void write_to_stream(pa_stream *p, size_t nbytes, int channel);


////////////////////////////////////////////////////////////////

/* Do one interation of the audio processing loop */
void audio_update(void)
{
    int i;
    pa_mainloop_iterate(e.priv.audio_loop,
                        0, /* 0 == nonblocking */
                        NULL);

    for(i = 0; i < TONE_COUNT; i++)
    {
        if(e.tone[i].start_trigger)
            start_tone(i);
        if(e.tone[i].stop_trigger)
            stop_tone(i);
    }
    for(i = 0; i < NOISE_COUNT; i++)
    {
        if(e.noise[i].start_trigger)
            start_noise(i);
        if(e.noise[i].stop_trigger)
            stop_noise(i);
    }
}


/* This callback gets called when our context changes state.  We
 * really only care about when it's ready or if it has failed. */
static void cb_audio_context_state(pa_context *c, void *userdata)
{
    e.priv.audio_state = (int) pa_context_get_state(c);
    switch(e.priv.audio_state)
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
        g_debug("PulseAudio set context state to %d", e.priv.audio_state);
        break;
    }
}

/* This callback is called when the server starts playback after an
 * underrun or on initial startup. */
void cb_audio_stream_started(pa_stream *p, void *userdata)
{
    int i = (int)userdata;
    g_debug("PulseAudio started playback on channel %d", i);
    set_channel_is_playing(i, TRUE);
}
    
/* This is called when new data may be written to the stream.  If we
  have data, we can ship it, otherwise we just note that the stream is
  waiting.  */
void cb_audio_stream_write(pa_stream *p, size_t nbytes, void *userdata)
{
    int channel = (int)userdata;
    size_t from_len, from_pos;
    
    g_assert(channel < CHANNEL_COUNT);
    
    g_debug("PulseAudio requested %d bytes on channel %d", nbytes, channel);
    
    from_len = e.priv.audio_len[channel];
    from_pos = e.priv.audio_pos[channel];

    if(e.priv.audio_len[channel] == 0 || e.priv.audio_pos[channel] == e.priv.audio_len[channel])
    {
        /* Don't have any data to give, so we note that this stream is
         * waiting for data and return. */
        set_channel_is_playing(channel, FALSE);
        e.priv.audio_waiting[channel] = nbytes;
    }
    else
    {
        e.priv.audio_waiting[channel] = 0;
        write_to_stream(p, nbytes, channel);
    }
}

static void generate_tone_data(double D_attack, double D_decay, double D_sustain, double D_release,
                               double F_initial, double F_attack, double F_sustain, double F_release,
                               double A_attack, double A_sustain,
                               double duty, _Bool noise,
                               uint8_t **buffer, size_t *length)
{
    /* D = duration in sec
       F = frequency in Hz
       A = amplitude, from 0.0 to 1.0 */
	double t, t_start, amplitude, frequency, period;
    double duration;
    int i, first;
    int level_a, level_b;

    *buffer = NULL;
    *length = 0;

    g_return_if_fail (F_initial >= 12.0 && F_initial < 22050);
    g_return_if_fail (D_attack >= 0.0);
    
    duration = D_attack + D_decay + D_sustain + D_release;
    *length = ceil(duration * (double) AUDIO_SAMPLE_RATE_IN_HZ);
    if(*length < MIN_AUDIO_SAMPLE_LENGTH)
        *length = MIN_AUDIO_SAMPLE_LENGTH;
    *buffer = g_new(uint8_t, *length);

    t = 0.0;
    t_start = 0.0;
    i = 0;
    period = 0.0;
    first = TRUE;
    while (i < *length)
    {
        if(first || t - t_start >= period)
        {
            first = FALSE;
            t_start = t;

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
                if(g_rand_boolean(e.priv.seed))
                    level_a = 128 + 127 * amplitude;
                else
                    level_a = 128 - 127 * amplitude;
                if(g_rand_boolean(e.priv.seed))
                    level_b = 128 + 127 * amplitude;
                else
                    level_b = 128 - 127 * amplitude;
            }
            else
            {
                level_a = 128 + 127 * amplitude;
                level_b = 128 - 127 * amplitude;
            }

        }
        if (t - t_start < period * duty)
            (*buffer)[i] = level_a;
        else if (t - t_start < period)
            (*buffer)[i] = level_b;
        i ++;
        t += 1.0 / (double) AUDIO_SAMPLE_RATE_IN_HZ;
    }
    {
        FILE *fp = fopen("wav.txt", "wt");
        int i;
        for(i = 0; i < *length; i++)
            fprintf(fp, "%d %d\n", i, (int)(*buffer)[i]);
        fclose(fp);
    }    
}

void initialize_audio()
{
    int ret;
    unsigned long pid;
    int i;

    /* Allocate a new mainloop object to handle the audio polling */
    e.priv.audio_loop = pa_mainloop_new();
    if (e.priv.audio_loop == NULL)
        g_error("Could not allocation PulseAudio mainloop");

    /* For some stupid reason, we can't use the mainloop structure
       directly to make a context.  We instead grab its vtable. */
    e.priv.audio_loop_vtable = pa_mainloop_get_api(e.priv.audio_loop);

    /* Property lists for PulseAudio */
    e.priv.audio_loop_proplist = pa_proplist_new();
    if(e.priv.audio_loop_proplist == NULL)
    {
        pa_mainloop_free(e.priv.audio_loop);
        g_error("Could not create a new PulseAudio mainloop property list");
    }
    
    /* I have no idea which properties must be set, if any.  */
    ret = pa_proplist_sets(e.priv.audio_loop_proplist, PA_PROP_MEDIA_ROLE, BURRO_PROP_MEDIA_ROLE);
    if (ret != 0)
        g_warning("Failed to set PulseAudio mainloop property '%s' to '%s'", PA_PROP_MEDIA_ROLE, BURRO_PROP_MEDIA_ROLE);
    ret = pa_proplist_sets(e.priv.audio_loop_proplist, PA_PROP_APPLICATION_ID, BURRO_PROP_APPLICATION_ID);
    if (ret != 0)
        g_warning("Failed to set PulseAudio mainloop property '%s' to '%s'",  PA_PROP_APPLICATION_ID, BURRO_PROP_APPLICATION_ID);
    pa_proplist_sets(e.priv.audio_loop_proplist, PA_PROP_APPLICATION_NAME, BURRO_PROP_APPLICATION_NAME);
    if (ret != 0)
        g_warning("Failed to set PulseAudio mainloop property '%s' to '%s'",  PA_PROP_APPLICATION_ID, BURRO_PROP_APPLICATION_ID);
    pid = getpid();
    ret = pa_proplist_setf (e.priv.audio_loop_proplist, PA_PROP_APPLICATION_PROCESS_ID, "%lu", pid);
    if (ret != 0)
        g_warning("Failed to set PulseAudio mainloop property '%s' to '%d'",  PA_PROP_APPLICATION_PROCESS_ID, pid);

    /* A context is the basic object for a connection to a PulseAudio
       server. It multiplexes commands, data streams and events
       through a single channel.

       There is no need for more than one context per application,
       unless connections to multiple servers are needed.

       Instantiate a new connection context with an abstract mainloop
       API and an application name, and specify the initial client
       property list.
    */
    e.priv.audio_context = pa_context_new_with_proplist(e.priv.audio_loop_vtable,
                                                        BURRO_PROP_APPLICATION_NAME,
                                                        e.priv.audio_loop_proplist);
    if (e.priv.audio_context == NULL)
    {
        pa_mainloop_free(e.priv.audio_loop);
        g_error("Could not create new PulseAudio context");
    }

    /* A context must be connected to a server before any operation
     * can be issued. Calling pa_context_connect() will initiate the
     * connection procedure. Unlike most asynchronous operations,
     * connecting does not result in a pa_operation object. Instead,
     * the application should register a callback using
     * pa_context_set_state_callback() */
    e.priv.audio_state = (int)PA_CONTEXT_UNCONNECTED;
    ret = pa_context_connect(e.priv.audio_context,
                             NULL, /* Default server*/ 
                             PA_CONTEXT_NOFLAGS, NULL);
    if(ret != 0)
        g_error("Failed to connect to the PulseAudio daemon");

    pa_context_set_state_callback(e.priv.audio_context,
                                  cb_audio_context_state, NULL);


    /* We wait (blocking) for the daemon to reply that the connection
     * is good. The value e.priv.audio_state is set in
     * cb_audio_context_state().  */
    while(TRUE)
    { 
        pa_mainloop_iterate(e.priv.audio_loop, 1, NULL);
        if (e.priv.audio_state == PA_CONTEXT_FAILED)
            g_error("Failed to ready the connection to the PulseAudio daemon");
        else if  (e.priv.audio_state == PA_CONTEXT_TERMINATED)
            g_error("Premature termination of the connection to the PulseAudio daemon");
        else if (e.priv.audio_state == PA_CONTEXT_READY)
        {
            g_debug("Connection to PulseAudio daemon is ready");
            break;
        }
    }

    for(i = 0; i < CHANNEL_COUNT; i++)
        initialize_channel(i);

    g_debug("Audio initialization complete");
}

/* The audio consists of a handful of channels.  Each channel has the
 * same bitrate and buffer properties.  */
void initialize_channel(int i)
{
    pa_sample_spec ss;
    pa_channel_map map;
    pa_buffer_attr buffer_attr;
    pa_proplist *pl;
    pa_stream *p;
    char *str;
    int ret;

    /* pa sample_spec is a structure whose fields you can directly set.
       3 fields: format, rate channels. */
    ss.rate = AUDIO_SAMPLE_RATE_IN_HZ;
    ss.channels = 1;
    ss.format = PA_SAMPLE_U8;
    
    /* pa_channel_map is a struct whose fields you can directly set.
       fields: channels, map[MAX]
       map is an enum, like PA_CHANNEL_POSITION_MONO or 
       PA_CHANNEL_POSITION_FRONT_LEFT 	
       PA_CHANNEL_POSITION_FRONT_RIGHT
       PA_CHANNEL_POSITION_FRONT_CENTER */
    memset(&map, 0, sizeof(map));
    map.channels = 1;
    map.map[0] = PA_CHANNEL_POSITION_MONO;
    
    /* Here we set the buffering behavior of the audio.  We don't
     * require much buffering. */
    buffer_attr.maxlength = -1; /* default */
    buffer_attr.prebuf = -1;
    buffer_attr.minreq = -1;
    buffer_attr.fragsize = -1;
    buffer_attr.tlength = AUDIO_SAMPLE_RATE_IN_HZ / 256; 

    /* Create a new, unconnected stream with the specified name
       and sample type, and specify the initial stream property list.
       PA_CONTEXT came from the main loop.
       NAME is whatever you want.
       PA_SAMPLE_SPEC is a rate/depth structure with fields that you set directly.
       PA_PROPLIST, is a key-value struct that you can cram full
       of stuff, but, it is unclear what is vital.  As far as I can tell, 
       PulseAudio itself sets the properties
       "stream-time"
       and queries
       PA_PROP_MEDIA_NAME
    */
    pl = pa_proplist_new();
    str = g_strdup_printf("Burro Game Engine, Channel %d", i);
    pa_proplist_sets(pl, PA_PROP_MEDIA_NAME, str);
    pa_proplist_sets(pl, PA_PROP_MEDIA_ROLE, "game");
    p = pa_stream_new_with_proplist(e.priv.audio_context,
                                    str, /* Stream name */
                                    &ss, /* Stream rate and depth */
                                    &map, /* Stream mono/stereo stuff */
                                    pl); /* Proplist */
    if(p == NULL)
        g_error("Failed to create new stream %d", i);
    g_free (str);

    pa_stream_set_started_callback(p, cb_audio_stream_started, (void *) i);
    pa_stream_set_write_callback(p, cb_audio_stream_write, (void *) i);

    /* Connect the stream to a sink,
       DEV and VOLUME are supposed to be NULL.
       DEV==NULL is the default sync.
       VOLUME==NULL is default volume.
       ATTR=NULL means we use default buffering.  (What is default buffering?)
       
       FLAGS can probably be PA_STREAM_NOFLAGS, or maybe
       PA_STREAM_START_CORKED for sync purporses.
       One example uses PA_STREAM_INTERPOLATE_TIMING
       |PA_STREAM_ADJUST_LATENCY
       |PA_STREAM_AUTO_TIMING_UPDATE
       
       SYNC_STREAM can be null, I guess, or maybe be the stream
       that you CORK with? */
    
    ret = pa_stream_connect_playback(p,
                                     NULL, /* Default sink device */
                                     &buffer_attr, /* Buffering attributes */
                                     PA_STREAM_NOFLAGS,
                                     NULL, /* Default volume */
                                     NULL); /* Not synced with another stream */
    if(ret < 0)
        g_error("Error connecting audio stream %d", i);

    e.priv.audio_stream[i] = p;
    e.priv.audio_waiting[i] = 0;
    e.priv.audio_buf[i] = NULL;
    e.priv.audio_len[i] = 0;
    e.priv.audio_pos[i] = 0;
}

void set_channel_is_playing(int channel, _Bool flag)
{
    if(channel < TONE_COUNT)
        e.tone[channel].is_playing = flag;
    else if(channel < TONE_COUNT + NOISE_COUNT)
        e.noise[channel - TONE_COUNT].is_playing = flag;
    else if(channel < TONE_COUNT + NOISE_COUNT + WAVE_COUNT)
        e.wave[channel - TONE_COUNT - NOISE_COUNT].is_playing = flag;
    else
        g_error("Bad channel index %d", channel);
}

static void start_noise(int i)
{
    uint8_t *buffer;
    size_t length;
    char *str;

    stop_noise(i);
    e.noise[i].start_trigger = FALSE;

    generate_tone_data(e.noise[i].attack_duration,
                       e.noise[i].decay_duration,
                       e.noise[i].sustain_duration,
                       e.noise[i].release_duration,
                       e.noise[i].initial_frequency,
                       e.noise[i].attack_frequency,
                       e.noise[i].sustain_frequency,
                       e.noise[i].release_frequency,
                       e.noise[i].attack_amplitude,
                       e.noise[i].sustain_amplitude,
                       e.noise[i].duty,
                       TRUE,
                       &buffer, &length);
    e.priv.audio_buf[NOISE(i)] = buffer;
    e.priv.audio_pos[NOISE(i)] = 0;
    e.priv.audio_len[NOISE(i)] = length;

    /* If the channel is waiting for data, ship it now. */
    if(e.priv.audio_waiting[NOISE(i)])
    {
        write_to_stream(e.priv.audio_stream[NOISE(i)],
                        e.priv.audio_waiting[NOISE(i)],
                        NOISE(i));
    }
}

static void start_tone(int i)
{
    uint8_t *buffer;
    size_t length;
    char *str;

    stop_tone(i);
    e.tone[i].start_trigger = FALSE;

    generate_tone_data(e.tone[i].attack_duration,
                       e.tone[i].decay_duration,
                       e.tone[i].sustain_duration,
                       e.tone[i].release_duration,
                       e.tone[i].initial_frequency,
                       e.tone[i].attack_frequency,
                       e.tone[i].sustain_frequency,
                       e.tone[i].release_frequency,
                       e.tone[i].attack_amplitude,
                       e.tone[i].sustain_amplitude,
                       e.tone[i].duty,
                       FALSE,
                       &buffer, &length);
    e.priv.audio_buf[TONE(i)] = buffer;
    e.priv.audio_pos[TONE(i)] = 0;
    e.priv.audio_len[TONE(i)] = length;

    /* If the channel is waiting for data, ship it now. */
    if(e.priv.audio_waiting[TONE(i)])
    {
        write_to_stream(e.priv.audio_stream[TONE(i)],
                        e.priv.audio_waiting[TONE(i)],
                        TONE(i));
    }
}

static void stop_noise(int i)
{
    /* Flush the playback buffer of this stream */
    pa_stream_flush(e.priv.audio_stream[NOISE(i)], NULL, NULL);
    g_free(e.priv.audio_buf[NOISE(i)]);
    e.priv.audio_buf[NOISE(i)] = NULL;
    e.noise[i].stop_trigger = FALSE;
    e.noise[i].is_playing = FALSE;
}

static void stop_tone(int i)
{
    /* Flush the playback buffer of this stream */
    pa_stream_flush(e.priv.audio_stream[TONE(i)], NULL, NULL);
    g_free(e.priv.audio_buf[TONE(i)]);
    e.priv.audio_buf[TONE(i)] = NULL;
    e.tone[i].stop_trigger = FALSE;
    e.tone[i].is_playing = FALSE;
}


/* Move data from the engine to the channel */
void write_to_stream(pa_stream *p, size_t nbytes, int channel)
{
    uint8_t *to_buf, *from_buf;
    size_t to_len, from_len, from_pos;
    int ret;
    
    g_assert(channel < CHANNEL_COUNT);
    if(e.priv.audio_len[channel] == 0 
       || (e.priv.audio_pos[channel] >= e.priv.audio_len[channel]))
        g_error("Tried to write to stream when no audio data was available");
    if(e.priv.audio_len[channel] - e.priv.audio_pos[channel] > nbytes)
    {
        g_debug("Engine sending fragment audio data to channel %d, %d to %d of %d",
                channel,
                e.priv.audio_pos[channel],
                e.priv.audio_pos[channel] + nbytes,
                e.priv.audio_len[channel]);
    
        /* writes Data to P.  Since we used pa_stream_begin_write on P
           already, DATA has to be that block. LENGTH needs to be
           passed, but, it hs to be less than the NBYTES returns by
           pa_stream_begin_write(). */
        ret = pa_stream_write(p, 
                              &e.priv.audio_buf[channel][e.priv.audio_pos[channel]], 
                              nbytes, 
                              NULL, 
                              0, 
                              PA_SEEK_RELATIVE);
        e.priv.audio_pos[channel] += nbytes;
    }
    else
    {
        g_debug("Engine sending remaining audio data to channel %d, %d to %d of %d",
                channel,
                e.priv.audio_pos[channel],
                e.priv.audio_len[channel],
                e.priv.audio_len[channel]);

        ret = pa_stream_write(p, 
                              &e.priv.audio_buf[channel][e.priv.audio_pos[channel]], 
                              e.priv.audio_len[channel] - e.priv.audio_pos[channel], 
                              NULL, 
                              0, 
                              PA_SEEK_RELATIVE);
        e.priv.audio_pos[channel] = e.priv.audio_len[channel];
    }

    e.priv.audio_waiting[channel] = 0;        
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
