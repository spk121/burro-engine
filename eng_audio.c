#include <math.h>
#include <gst/gst.h>
#include "engine.h"
#include "eng_audio.h"

static void initialize_audio_source (GstElement *pipeline, GstElement *adder, GstElement **src, char *id_name, int id_number);
static void generate_tone_data(double D_attack, double D_decay, double D_sustain, double D_release,
                               double F_initial, double F_attack, double F_sustain, double F_release,
                               double A_attack, double A_sustain,
                               double duty, _Bool noise,
                               uint8_t **buffer, size_t *length);
static gboolean noise_bus_cb (GstBus *bus, GstMessage *message, gpointer channel);
static void start_tone(int i);
static void stop_tone(int i);
static void start_noise(int i);
static void stop_noise(int i);
static gboolean tone_bus_cb (GstBus *bus, GstMessage *message, gpointer channel);

void initialize_audio()
{
}

static void initialize_audio_source (GstElement *pipeline, GstElement *adder, GstElement **tone, char *id_name, int id_number)
{
}

void audio_update(void)
{
    int i;
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


void eng_audio_fini ()
{
    GST_DEBUG ("stopping");

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
                /* Shouldn't get here */
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

/*
 * EOS will be called when the src element has an end of stream.
 * Note that this function will be called in the thread context.
 * We will place an idle handler to punt this back to the main thread.
 */
static gboolean noise_bus_cb (GstBus *bus, GstMessage *message, gpointer channel)
{
    switch(GST_MESSAGE_TYPE(message))
    {
    case GST_MESSAGE_EOS:
        e.noise[(int)channel].is_playing = FALSE;
        g_idle_add ((GSourceFunc) eos_cb, channel + TONE_COUNT);

        /* Done waiting for EOS */
        return FALSE;
        break;
    }
 
    /* Keep waiting for eos */
    return TRUE;
}

static void start_noise(int i)
{
	GMemoryInputStream *mistream;
	GstElement *source, *sink, *pipeline;
	GstPad *sourcepad;
	GMainLoop *loop;
    GstBus *bus;
    uint8_t *buffer;
    size_t length;
    char *str;

    if(e.noise[i].is_playing == TRUE)
        stop_noise(i);
    e.noise[i].start_trigger = FALSE;
    e.noise[i].is_playing = TRUE;

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
	mistream = G_MEMORY_INPUT_STREAM(g_memory_input_stream_new_from_data(buffer, length,
                                                                         (GDestroyNotify)g_free));
    str = g_strdup_printf("noise-source%d", i);
    e.priv.noise_source[i] = gst_element_factory_make("giostreamsrc", str);
    g_free(str);
    g_object_set(G_OBJECT(e.priv.noise_source[i]), "stream", G_INPUT_STREAM(mistream), NULL);
    sourcepad = gst_element_get_static_pad(e.priv.noise_source[i], "src");
	gst_pad_set_caps (sourcepad,
                      gst_caps_new_simple ("audio/x-raw-int",
                                           "rate", G_TYPE_INT, 22000,
                                           "channels", G_TYPE_INT, 1,
                                           "width", G_TYPE_INT, 8,
                                           "depth", G_TYPE_INT, 8,
                                           "signed", G_TYPE_BOOLEAN, FALSE,
                                           NULL));
	gst_object_unref (sourcepad);
    str = g_strdup_printf("noise-sink%d", i);
    e.priv.noise_sink[i] = gst_element_factory_make("pulsesink", str);
    g_free(str);
    str = g_strdup_printf("noise-pipeline%d", i);
    e.priv.noise_pipeline[i] = gst_pipeline_new(str);
    g_free(str);
    gst_bin_add_many(GST_BIN(e.priv.noise_pipeline[i]),
                     e.priv.noise_source[i],
                     e.priv.noise_sink[i],
                     NULL);
    gst_element_link_many(e.priv.noise_source[i],
                          e.priv.noise_sink[i],
                          NULL);
    gst_element_set_state(e.priv.noise_pipeline[i],
                          GST_STATE_PLAYING);
    bus = gst_pipeline_get_bus(GST_PIPELINE(e.priv.noise_pipeline[i]));
    gst_bus_add_watch(bus, noise_bus_cb, i);
    gst_object_unref(bus);
}

static void start_tone(int i)
{
	GMemoryInputStream *mistream;
	GstElement *source, *sink, *pipeline;
	GstPad *sourcepad;
	GMainLoop *loop;
    GstBus *bus;
    uint8_t *buffer;
    size_t length;
    char *str;

    if(e.tone[i].is_playing == TRUE)
        stop_tone(i);
    e.tone[i].start_trigger = FALSE;
    e.tone[i].is_playing = TRUE;

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
	mistream = G_MEMORY_INPUT_STREAM(g_memory_input_stream_new_from_data(buffer, length,
                                                                         (GDestroyNotify)g_free));
    str = g_strdup_printf("tone-source%d", i);
    e.priv.tone_source[i] = gst_element_factory_make("giostreamsrc", str);
    g_free(str);
    g_object_set(G_OBJECT(e.priv.tone_source[i]), "stream", G_INPUT_STREAM(mistream), NULL);
    sourcepad = gst_element_get_static_pad(e.priv.tone_source[i], "src");
	gst_pad_set_caps (sourcepad,
                      gst_caps_new_simple ("audio/x-raw-int",
                                           "rate", G_TYPE_INT, 22000,
                                           "channels", G_TYPE_INT, 1,
                                           "width", G_TYPE_INT, 8,
                                           "depth", G_TYPE_INT, 8,
                                           "signed", G_TYPE_BOOLEAN, FALSE,
                                           NULL));
	gst_object_unref (sourcepad);
    str = g_strdup_printf("tone-sink%d", i);
    e.priv.tone_sink[i] = gst_element_factory_make("pulsesink", str);
    g_free(str);
    str = g_strdup_printf("tone-pipeline%d", i);
    e.priv.tone_pipeline[i] = gst_pipeline_new(str);
    g_free(str);
    gst_bin_add_many(GST_BIN(e.priv.tone_pipeline[i]),
                     e.priv.tone_source[i],
                     e.priv.tone_sink[i],
                     NULL);
    gst_element_link_many(e.priv.tone_source[i],
                          e.priv.tone_sink[i],
                          NULL);
    gst_element_set_state(e.priv.tone_pipeline[i],
                          GST_STATE_PLAYING);

    /* Apparently the EOS signal is specifically emitted from a pipeline's bus. */
    bus = gst_pipeline_get_bus(GST_PIPELINE(e.priv.tone_pipeline[i]));
    gst_bus_add_watch(bus, tone_bus_cb, i);
    gst_object_unref(bus);
}

static void stop_noise(int i)
{
    e.noise[i].stop_trigger = FALSE;
    e.noise[i].is_playing = FALSE; 
    if (e.priv.noise_pipeline != NULL)
    {
        gst_element_set_state(e.priv.noise_pipeline[i],
                              GST_STATE_NULL);
        gst_object_unref(GST_OBJECT(e.priv.noise_pipeline[i]));
        e.priv.noise_source[i] = NULL;
        e.priv.noise_sink[i] = NULL;
        e.priv.noise_pipeline[i] = NULL;
    }
}

static void stop_tone(int i)
{
    e.tone[i].stop_trigger = FALSE;
    e.tone[i].is_playing = FALSE; 
    if (e.priv.tone_pipeline != NULL)
    {
        gst_element_set_state(e.priv.tone_pipeline[i],
                              GST_STATE_NULL);
        gst_object_unref(GST_OBJECT(e.priv.tone_pipeline[i]));
        e.priv.tone_source[i] = NULL;
        e.priv.tone_sink[i] = NULL;
        e.priv.tone_pipeline[i] = NULL;
    }
}

/*
 * EOS will be called when the src element has an end of stream.
 * Note that this function will be called in the thread context.
 * We will place an idle handler to punt this back to the main thread.
 */
static gboolean tone_bus_cb (GstBus *bus, GstMessage *message, gpointer channel)
{
    switch(GST_MESSAGE_TYPE(message))
    {
    case GST_MESSAGE_EOS:
        e.tone[(int)channel].is_playing = FALSE;
        g_idle_add ((GSourceFunc) eos_cb, channel);

        /* Done waiting for EOS */
        return FALSE;
        break;
    }
 
    /* Keep waiting for eos */
    return TRUE;
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
