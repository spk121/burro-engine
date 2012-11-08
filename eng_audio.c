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
static void start_tone(int i);
static void stop_tone(int i) {}
static void start_noise(int i) {}
static void stop_noise(int i) {}

void initialize_audio()
{
    int i, count;
    char *name;

    e.priv.pipeline = gst_pipeline_new("pipeline");

    e.priv.adder = gst_element_factory_make("adder", "adder");
    gst_bin_add(GST_BIN (e.priv.pipeline), e.priv.adder);
    e.priv.sink = gst_element_factory_make("alsasink", "sink");
    gst_bin_add(GST_BIN (e.priv.pipeline), e.priv.sink);

    /* The mixing element connects to the output */
    gst_element_link(e.priv.adder, e.priv.sink);

    count = 0;
    for (i = 0; i < TONE_COUNT; i ++)
    {
        initialize_audio_source(e.priv.pipeline, e.priv.adder, &(e.priv.tone[i]), "tone%d", i);
        count ++;
    }
    for (i = 0; i < NOISE_COUNT; i ++)
    {
        initialize_audio_source(e.priv.pipeline, e.priv.adder, &(e.priv.noise[i]), "noise%d", i);
        count ++;
    }
    for (i = 0; i < WAVE_COUNT; i ++)
    {
        initialize_audio_source(e.priv.pipeline, e.priv.adder, &(e.priv.wave[i]), "wave%d", i);
        count ++;
    }
	gst_element_set_state (e.priv.pipeline, GST_STATE_NULL);

}

static void initialize_audio_source (GstElement *pipeline, GstElement *adder, GstElement **tone, char *id_name, int id_number)
{
    char *name;
    GstPad *srcpad, *sinkpad;

    name = g_strdup_printf(id_name, id_number);
    *tone = gst_element_factory_make ("giostreamsrc", name);
	srcpad = gst_element_get_static_pad(*tone, "src");
    sinkpad = gst_element_get_request_pad(adder, "sink%d");

    gst_pad_set_caps (srcpad,
                      gst_caps_new_simple ("audio/x-raw-int",
                                           "rate", G_TYPE_INT, AUDIO_SAMPLE_RATE_IN_HZ,
                                           "channels", G_TYPE_INT, 1,
                                           "width", G_TYPE_INT, 8,
                                           "depth", G_TYPE_INT, 8,
                                           "signed", G_TYPE_BOOLEAN, FALSE,
                                           NULL));
    gst_bin_add(GST_BIN (pipeline), *tone);
    gst_pad_link(srcpad, sinkpad);
    gst_object_unref(GST_OBJECT (srcpad));
    gst_object_unref(GST_OBJECT (sinkpad));
    g_free(name);
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

    gst_element_set_state (e.priv.pipeline, GST_STATE_NULL);

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
            period = 1.0 / frequency;
            t_start = t;
            first = FALSE;
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
}

static void start_tone(int i)
{
	GMemoryInputStream *mistream;
	GstElement *source, *sink, *pipeline;
	GstPad *sourcepad;
	GMainLoop *loop;
    uint8_t *buffer;
    size_t length;

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
    
	g_object_set (G_OBJECT (e.priv.tone[i]), "stream", G_INPUT_STREAM (mistream), NULL);
    gst_element_set_state (e.priv.sink, GST_STATE_PLAYING);
    gst_element_set_state (e.priv.adder, GST_STATE_PLAYING);
    gst_element_set_state (e.priv.tone[i], GST_STATE_PLAYING);
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
