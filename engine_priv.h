#ifndef ENGINE_PRIV_H_INCLUDED
#define ENGINE_PRIV_H_INCLUDED

#include <gtk/gtk.h>
#include <cairo.h>
#include <gst/gst.h>
#include "engine_constants.h"

/* These are the internal states of the engine */
struct priv_entry
{
    /* The volume of heat juice flowing from my head, in milliliters. */
    float philogistan;

    /* Glib's random seed */
    GRand *seed;
    
    /* For the GUI */
    GtkWidget *window;
    GtkWidget *fixed;
    GtkWidget *main_screen;
    GtkWidget *sub_screen;
    GMainLoop *main_loop;

    cairo_t *main_screen_context;
    cairo_surface_t *main_screen_surface;
    cairo_t *sub_screen_context;
    cairo_surface_t *sub_screen_surface;

    /* For frames-per-second calculation */
    GTimer *timer;
    int update_count;
    int draw_count;
    double before_update_time, after_update_time;
    double before_draw_time, after_draw_time;
    double hundred_frames_draw_time;

    /* Basic state flags */
    _Bool initialized_flag;
    _Bool minimized_flag;
    _Bool active_flag;
    _Bool quitting_flag;
    _Bool run_full_speed_flag;

    /* Audio engine */
    GstElement *tone_pipeline[TONE_COUNT], *tone_source[TONE_COUNT], *tone_sink[TONE_COUNT];
    GstElement *noise_pipeline[NOISE_COUNT], *noise_source[NOISE_COUNT], *noise_sink[NOISE_COUNT];
    GstElement *wave_pipeline[WAVE_COUNT], *wave_source[WAVE_COUNT], *wave_sink[WAVE_COUNT];
};


#endif // ENGINE_PRIV_H_INCLUDED

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
