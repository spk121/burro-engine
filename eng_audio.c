#include <gst/gst.h>
#include "engine.h"
#include "eng_audio.h"

static void initialize_audio_source (GstElement *pipeline, GstElement *adder, GstElement *src, char *id_name, int id_number);

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
        initialize_audio_source(e.priv.pipeline, e.priv.adder, e.priv.tone[i], "tone%d", i);
        count ++;
    }
    for (i = 0; i < NOISE_COUNT; i ++)
    {
        initialize_audio_source(e.priv.pipeline, e.priv.adder, e.priv.noise[i], "noise%d", i);
        count ++;
    }
    for (i = 0; i < WAVE_COUNT; i ++)
    {
        initialize_audio_source(e.priv.pipeline, e.priv.adder, e.priv.wave[i], "wave%d", i);
        count ++;
    }
	gst_element_set_state (e.priv.pipeline, GST_STATE_NULL);

}

static void initialize_audio_source (GstElement *pipeline, GstElement *adder, GstElement *tone, char *id_name, int id_number)
{
    char *name;
    GstPad *srcpad, *sinkpad;

    name = g_strdup_printf(id_name, id_number);
    tone = gst_element_factory_make ("giostreamsrc", name);
	srcpad = gst_element_get_static_pad(tone, "src");
    sinkpad = gst_element_get_request_pad(adder, "sink%d");

    gst_pad_set_caps (srcpad,
                      gst_caps_new_simple ("audio/x-raw-int",
                                           "rate", G_TYPE_INT, AUDIO_SAMPLE_RATE_IN_HZ,
                                           "channels", G_TYPE_INT, 1,
                                           "width", G_TYPE_INT, 8,
                                           "depth", G_TYPE_INT, 8,
                                           "signed", G_TYPE_BOOLEAN, FALSE,
                                           NULL));
    gst_pad_link(srcpad, sinkpad);
    gst_bin_add(GST_BIN (pipeline), tone);
    gst_object_unref(GST_OBJECT (srcpad));
    gst_object_unref(GST_OBJECT (sinkpad));
    g_free(name);
}




void eng_audio_fini ()
{
    GST_DEBUG ("stopping");

    gst_element_set_state (e.priv.pipeline, GST_STATE_NULL);

}

void generate_tone_data(double frequency, double amplitude, double duration, double duty,
                        double sweep_delta_frequency, double sweep_duration,
                        double envelope_delta_amplitude, double envelope_duration,
                        uint8_t *buffer, size_t *length)
{
    double t, a, x;
    double period, cycle_t;

    period = 1.0 / frequency;
    *length = ceil(duration * (double) AUDIO_SAMPLE_RATE_IN_HZ);
    buffer = g_new (uint8_t, *length);
    for (t = 0.0, cycle_t = 0.0; t < duration; t += 1.0 / AUDIO_SAMPLE_RATE_IN_HZ, cycle_t += 1.0 / AUDIO_SAMPLE_RATE_IN_HZ)
    {
        if (cycle_t > period)
            cycle_t -= period;

    }
}
