#include <stdint.h>
#include <gst/gst.h>
#include <gio/gio.h>

#define SAMPLE_RATE 22000
#define FREQ 440
#define DURATION 2
#define LEN (SAMPLE_RATE * DURATION)

int timer_callback (const void *data)
{
	g_main_loop_quit ((GMainLoop *) data);
	return FALSE;
}

int main (int argc, char ** argv)
{
	int i;
	uint8_t *wave;
	GMemoryInputStream *mistream;
	GstElement *source, *sink, *pipeline;
	GstPad *sourcepad;
	GMainLoop *loop;

	gst_init (&argc, &argv);

	wave = g_new (uint8_t, LEN);
	for (i = 0; i < LEN; i ++)
	{
		if (i % 50 < 25)
			wave[i] = 255;
		else
			wave[i] = 0;
	}
	mistream = G_MEMORY_INPUT_STREAM(g_memory_input_stream_new_from_data(wave,
									     LEN,
									     (GDestroyNotify) g_free));

	source = gst_element_factory_make ("giostreamsrc", "source");
	g_object_set (G_OBJECT (source), "stream", G_INPUT_STREAM (mistream), NULL);
	sourcepad = gst_element_get_static_pad(source, "src");
	gst_pad_set_caps (sourcepad,
			  gst_caps_new_simple ("audio/x-raw-int",
					       "rate", G_TYPE_INT, 22000,
					       "channels", G_TYPE_INT, 1,
					       "width", G_TYPE_INT, 8,
					       "depth", G_TYPE_INT, 8,
					       "signed", G_TYPE_BOOLEAN, FALSE,
					       NULL));
	gst_object_unref (sourcepad);
  
	sink = gst_element_factory_make ("alsasink", "sink");
  
	pipeline = gst_pipeline_new ("beep-player");

	gst_bin_add_many (GST_BIN (pipeline),
			  source, sink, NULL);
	gst_element_link_many (source, sink, NULL);
	gst_element_set_state (pipeline, GST_STATE_PLAYING);

	loop = g_main_loop_new (NULL, FALSE);
	g_timeout_add (2500, (GSourceFunc) timer_callback, loop);
	g_main_loop_run (loop);

	gst_element_set_state (pipeline, GST_STATE_NULL);

	gst_object_unref (GST_OBJECT (pipeline));
	g_main_loop_unref (loop);

	return 0;
}
