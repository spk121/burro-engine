/* This is audio engine's backend for using PulseAudio.
   Any pulseaudio specific calls go here. */
#include <pulse/pulseaudio.h>
#include <glib.h>
#include <string.h>
#include "pulseaudio.h"
#include "engine.h"

#define BURRO_PROP_MEDIA_ROLE "game"
#define BURRO_PROP_APPLICATION_ID "com.lonelycactus.projectburro"
#define BURRO_PROP_APPLICATION_NAME "ProjectBurro"

typedef struct pulse_priv_tag {
    int state;
    pa_mainloop *loop;
    _Bool finalize;
    int samples_written;
} pulse_priv_t;
static pulse_priv_t pulse;

static void cb_audio_context_state(pa_context *c, void *userdata);
void cb_audio_stream_started(pa_stream *p,
                                 void *userdata);
void cb_audio_stream_write(pa_stream *p, size_t nbytes, void *userdata);

/* This callback gets called when our context changes state.  We
 * really only care about when it's ready or if it has failed. */
static void cb_audio_context_state(pa_context *c, void *userdata)
{
    pulse.state = (int) pa_context_get_state(c);
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
void cb_audio_stream_started(pa_stream *p, void *userdata)
{
    g_debug("PulseAudio started playback");
}
    
/* This is called when new data may be written to the stream.  If we
  have data, we can ship it, otherwise we just note that the stream is
  waiting.  */
void cb_audio_stream_write(pa_stream *p, size_t nbytes, void *userdata)
{
    size_t from_len, from_pos;
    int ret;
    int n = nbytes;
    int16_t y;
    uint8_t *buf;
    int i;
    
    buf = g_new(uint8_t, nbytes);
    for(i = 0; i < n; i ++)
    {
        if(e.priv.audio_count[i] > 0)
            // y = e.priv.audio_buf[i] / e.priv.audio_count[i] - INT8_MIN;
            y = e.priv.audio_buf[i] / 5 - INT8_MIN;
        else
            y = -INT8_MIN;
        if(y > UINT8_MAX)
        {
            y = UINT8_MAX;
            g_warning("Numeric overflow in cb_audio_stream_write");
        }
        else if (y < 0)
        {
            y = 0;
            g_warning("Numeric underflow in cb_audio_stream_write");
        }
        buf[i] = y;
    }
    ret = pa_stream_write(p, 
                          buf, 
                          n, 
                          NULL, 
                          0, 
                          PA_SEEK_RELATIVE);
    g_free(buf);

    g_memmove(e.priv.audio_buf, e.priv.audio_buf + n, sizeof(int16_t) * (AUDIO_BUFFER_SIZE - n));
    memset(e.priv.audio_buf + AUDIO_BUFFER_SIZE - n, 0, n);
    g_memmove(e.priv.audio_count, e.priv.audio_count + n, sizeof(int8_t) * (AUDIO_BUFFER_SIZE - n));
    memset(e.priv.audio_count + AUDIO_BUFFER_SIZE - n, 0, n);

    pulse.samples_written += n;
}

void pulse_initialize_audio()
{
    pa_mainloop_api *vtable;
    pa_proplist *main_proplist, *stream_proplist;
    pa_context *context;
    int ret;
    int i;
    pa_sample_spec sample_specification;
    pa_channel_map channel_map;
    pa_buffer_attr buffer_attributes;
    pa_stream *stream;

    memset(&pulse, 0, sizeof(pulse));

    /* Allocate a new mainloop object to handle the audio polling */
    pulse.loop = pa_mainloop_new();

    if (pulse.loop == NULL)
        g_error("Could not allocation PulseAudio mainloop");

    /* For some stupid reason, we can't use the mainloop structure
       directly to make a context.  We instead grab its vtable. */
    vtable = pa_mainloop_get_api(pulse.loop);

    /* Property lists for PulseAudio */
    main_proplist = pa_proplist_new();
    if(main_proplist == NULL)
    {
        pa_mainloop_free(pulse.loop);
        g_error("Could not create a new PulseAudio mainloop property list");
    }
    
    /* Only the PA_PROP_MEDIA_ROLE is important.  */
    ret = pa_proplist_sets(main_proplist, PA_PROP_MEDIA_ROLE, BURRO_PROP_MEDIA_ROLE);
    if (ret != 0)
        g_warning("Failed to set PulseAudio mainloop property '%s' to '%s'", PA_PROP_MEDIA_ROLE, BURRO_PROP_MEDIA_ROLE);
    ret = pa_proplist_sets(main_proplist, PA_PROP_APPLICATION_ID, BURRO_PROP_APPLICATION_ID);
    if (ret != 0)
        g_warning("Failed to set PulseAudio mainloop property '%s' to '%s'",  PA_PROP_APPLICATION_ID, BURRO_PROP_APPLICATION_ID);
    pa_proplist_sets(main_proplist, PA_PROP_APPLICATION_NAME, BURRO_PROP_APPLICATION_NAME);
    if (ret != 0)
        g_warning("Failed to set PulseAudio mainloop property '%s' to '%s'",  PA_PROP_APPLICATION_ID, BURRO_PROP_APPLICATION_ID);

    /* A context is the basic object for a connection to a PulseAudio
       server. It multiplexes commands, data streams and events
       through a single channel.

       There is no need for more than one context per application,
       unless connections to multiple servers are needed.

       Instantiate a new connection context with an abstract mainloop
       API and an application name, and specify the initial client
       property list.
    */
    context = pa_context_new_with_proplist(vtable,
                                           BURRO_PROP_APPLICATION_NAME,
                                           main_proplist);
    pa_proplist_free(main_proplist);
    if (context == NULL)
    {
        pa_mainloop_free(pulse.loop);        
        g_error("Could not create new PulseAudio context");
    }

    /* A context must be connected to a server before any operation
     * can be issued. Calling pa_context_connect() will initiate the
     * connection procedure. Unlike most asynchronous operations,
     * connecting does not result in a pa_operation object. Instead,
     * the application should register a callback using
     * pa_context_set_state_callback() */
    pulse.state = PA_CONTEXT_UNCONNECTED;
    ret = pa_context_connect(context,
                             NULL, /* Default server*/ 
                             PA_CONTEXT_NOFLAGS, NULL);
    if(ret != 0)
    {
        g_error("Failed to connect to the PulseAudio daemon");
    }

    pa_context_set_state_callback(context,
                                  cb_audio_context_state, NULL);


    /* We wait (blocking) for the daemon to reply that the connection
     * is good. The value e.priv.audio_state is set in
     * cb_audio_context_state().  */
    while(TRUE)
    { 
        pa_mainloop_iterate(pulse.loop, 1, NULL);
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

    /* SAMPLE_SPEC:  we describe the data going into our channel */
    sample_specification.rate = AUDIO_SAMPLE_RATE_IN_HZ;
    sample_specification.channels = 1;
    sample_specification.format = PA_SAMPLE_U8;

    /* CHANNEL_MAP: Then we say which speakers we're using.  The game is mono, so
       that makes it simple. */
    memset(&channel_map, 0, sizeof(channel_map));
    channel_map.channels = 1;
    channel_map.map[0] = PA_CHANNEL_POSITION_MONO;

    /* BUFFER_ATTRIBUTES: Here we set the buffering behavior of the
     * audio.  We want low latency.

       One wiki suggests that to set a specific latency, 
       1. use pa_usec_to_bytes(&ss, ...) to convert the latency from a time unit to bytes
       2. use the PA_STREAM_ADJUST_LATENCY flag
       3. set pa_buffer_attr::tlength to latency in samples
       4. set rest of pa_buffer_attr to -1

       http://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/LatencyControl
    */
    
    buffer_attributes.tlength = pa_usec_to_bytes(AUDIO_LATENCY_REQUESTED_IN_MILLISECONDS * MICROSECONDS_PER_MILLISECOND,
                                                 &sample_specification);
    //buffer_attributes.tlength = pa_usec_to_bytes(1000 * MICROSECONDS_PER_MILLISECOND,
    //                                             &sample_specification);
    buffer_attributes.maxlength = -1; /* -1 == default */
    buffer_attributes.prebuf = -1;
    buffer_attributes.minreq = -1;
    buffer_attributes.fragsize = -1;

    /* PROPERTY_LIST: Then we set up a structure to hold PulseAudio
     * properties for this. */
    stream_proplist = pa_proplist_new();
    pa_proplist_sets(stream_proplist, PA_PROP_MEDIA_NAME, "mono channel");
    pa_proplist_sets(stream_proplist, PA_PROP_MEDIA_ROLE, "game");

    /* STREAM: Group everything together as a stream */
    stream = pa_stream_new_with_proplist(context,
                                         "mono channel", /* Stream name */
                                         &sample_specification,
                                         &channel_map,
                                         stream_proplist); /* Proplist */
    if(stream == NULL)
        g_error("Failed to create new stream");
    
    pa_stream_set_started_callback(stream, cb_audio_stream_started, NULL);
    pa_stream_set_write_callback(stream, cb_audio_stream_write, NULL);
    
    /* Connect the stream to the audio loop */
    ret = pa_stream_connect_playback(stream,
                                     NULL, /* Default sink device */
                                     &buffer_attributes,
                                     PA_STREAM_ADJUST_LATENCY,
                                     NULL, /* Default volume */
                                     NULL); /* Not synced with another stream */
    if(ret < 0)
        g_error("Error connecting audio stream to server");

    /* Finally done! */

    g_debug("PulseAudio initialization complete");
}

/* This finalizer is called if we are shutting down
   cleanly */
void pulse_finalize_audio()
{
    pa_mainloop_free(pulse.loop);
    pulse.finalize = TRUE;
    g_debug("PulseAudio finalization complete");
}

void pulse_update_audio()
{
    pulse.samples_written = 0;
    while (pa_mainloop_iterate(pulse.loop, 0, NULL) > 0)
        ;
    if (pulse.samples_written > 0)
        g_debug("Wrote %d samples to PulseAudio", pulse.samples_written);

}

/* This finalizer is called on EXIT */
void pulse_atexit_audio()
{
    if(!pulse.finalize)
    {
    }
    g_debug("PulseAudio atexit complete");
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
