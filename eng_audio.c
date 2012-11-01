#include <gst/gst.h>
#include "engine.h"
#include "eng_audio.h"

GstElement *pipeline;
GstElement *appsrc;

guint sourceid;

GTimer *timer;




With much thanks to the gurus on IRC/freenode/#gstreamer (especially __tim!), I've figured out how to fix the bugs in your sample code.

Bug 1. The last line of read_data() should "return TRUE" so that this function remains on the bus, and will be called repeatedly. Only return FALSE when you want to stop sending data completely.

Bug 2. Replace gst_caps_new_simple() with gst_video_format_new_caps(). You'll also need to include gst/video/video.h, and link against -lgstvideo-0.10.

Here's my version of the code, showing both how to display the images and how to stream an theora-encoded version through UDP. Make note of the usage of videorate for the latter case, since theora expects a constant frame rate source.

#include <gst/gst.h>
#include <gst/app/gstappsrc.h>
#include <gst/video/video.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <gdk-pixbuf/gdk-pixbuf.h>

GST_DEBUG_CATEGORY (appsrc_pipeline_debug);
#define GST_CAT_DEFAULT appsrc_pipeline_debug

typedef struct _App App;

struct _App
{
  GstElement *pipeline;
  GstElement *appsrc;

  GMainLoop *loop;
  guint sourceid;

  GTimer *timer;

};

App s_app;

static gboolean
read_data (App * app)
{
    guint len;
    GstFlowReturn ret;
    gdouble ms;

    ms = g_timer_elapsed(app->timer, NULL);
    if (ms > 1.0/20.0) {
        GstBuffer *buffer;
        GdkPixbuf *pb;
        gboolean ok = TRUE;

        buffer = gst_buffer_new();

        pb = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, 640, 480);
        gdk_pixbuf_fill(pb, 0xffffffff);

        GST_BUFFER_DATA (buffer) = gdk_pixbuf_get_pixels(pb);
        GST_BUFFER_SIZE (buffer) = 640*480*3*sizeof(guchar);

        GST_DEBUG ("feed buffer");
        g_signal_emit_by_name (app->appsrc, "push-buffer", buffer, &ret);
        gst_buffer_unref (buffer);

        if (ret != GST_FLOW_OK) {
            /* some error, stop sending data */
            GST_DEBUG ("some error");
            ok = FALSE;
        }

        g_timer_start(app->timer);

        return ok;
    }

    // g_signal_emit_by_name (app->appsrc, "end-of-stream", &ret);
    return TRUE;
}

/* This signal callback is called when appsrc needs data, we add an idle handler
* to the mainloop to start pushing data into the appsrc */
static void
start_feed (GstElement * pipeline, guint size, App * app)
{
  if (app->sourceid == 0) {
    GST_DEBUG ("start feeding");
    app->sourceid = g_idle_add ((GSourceFunc) read_data, app);
  }
}

/* This callback is called when appsrc has enough data and we can stop sending.
* We remove the idle handler from the mainloop */
static void
stop_feed (GstElement * pipeline, App * app)
{
  if (app->sourceid != 0) {
    GST_DEBUG ("stop feeding");
    g_source_remove (app->sourceid);
    app->sourceid = 0;
  }
}

static gboolean
bus_message (GstBus * bus, GstMessage * message, void * dummy)
{
  GST_DEBUG ("got message %s",
      gst_message_type_get_name (GST_MESSAGE_TYPE (message)));

  switch (GST_MESSAGE_TYPE (message)) {
    case GST_MESSAGE_ERROR: {
        GError *err = NULL;
        gchar *dbg_info = NULL;

        gst_message_parse_error (message, &err, &dbg_info);
        g_printerr ("ERROR from element %s: %s\n",
            GST_OBJECT_NAME (message->src), err->message);
        g_printerr ("Debugging info: %s\n", (dbg_info) ? dbg_info : "none");
        g_error_free (err);
        g_free (dbg_info);
        //g_main_loop_quit (app->loop);
        break;
    }
    case GST_MESSAGE_EOS:
      //g_main_loop_quit (app->loop);
      break;
    default:
      break;
  }
  return TRUE;
}

void eng_audio_init ()
{
      GError *error = NULL;
  GstBus *bus;
  GstCaps *caps;

    gst_init (0, NULL);
 pipeline = gst_parse_launch("appsrc name=mysource ! video/x-raw-rgb,width=640,height=480 ! ffmpegcolorspace ! videoscale method=1 ! xvimagesink", NULL);
 bus = gst_pipeline_get_bus (GST_PIPELINE (pipeline));

 /* add watch for messages */
  gst_bus_add_watch (bus, (GstBusFunc) bus_message, NULL);

 /* get the appsrc */
    app->appsrc = gst_bin_get_by_name (GST_BIN(pipeline), "mysource");
    g_signal_connect (appsrc, "need-data", G_CALLBACK (start_feed), NULL);
    g_signal_connect (appsrc, "enough-data", G_CALLBACK (stop_feed), NULL);

  /* set the caps on the source */
  caps = gst_video_format_new_caps(GST_VIDEO_FORMAT_RGB, 640, 480, 0, 1, 4, 3);
  gst_app_src_set_caps(GST_APP_SRC(app->appsrc), caps);


  /* go to playing and wait in a mainloop. */
  gst_element_set_state (app->pipeline, GST_STATE_PLAYING)

}

void eng_audio_fini ()
{
     GST_DEBUG ("stopping");

  gst_element_set_state (app->pipeline, GST_STATE_NULL);

  gst_object_unref (bus);
  g_main_loop_unref (app->loop);
}

void generate_tone_data(double frequency, double amplitude, double duration, double duty,
                        double sweep_delta_frequency, double sweep_duration,
                        double envelope_delta_amplitude, double envelope_duration,
                        uint8_t *buffer, size_t *length)
{
    double t, a, x;
    double period, cycle_t;

    period = 1.0 / frequency;
    length = ceil(duration * (double) SAMPLE_RATE);
    buffer = g_new (uint8_t, length);
    for (t = 0.0, cycle_t = 0.0; t < duration; t += 1.0 / SAMPLE_RATE, cycle_t += 1.0 / SAMPLE_RATE)
    {
        if (cycle_t > period)
            cycle_t -= period;

    }
}
