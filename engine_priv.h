#ifndef ENGINE_PRIV_H_INCLUDED
#define ENGINE_PRIV_H_INCLUDED

#include <cairo.h>

/* These are the internal states of the engine */
struct priv_entry
{
    /* The volume of heat juice flowing from my head, in milliliters. */
    float philogistan;

    /* For the GUI */
    GtkWidget *window;
    GtkWidget *fixed;
    GtkWidget *main_screen;
    GtkWidget *sub_screen;

    cairo_t *main_screen_context;
    cairo_surface_t *main_screen_surface;
    cairo_t *sub_screen_context;
    cairo_surface_t *sub_screen_surface;

    /* For frames-per-second calculation */
    int frame_count;
    GTimer *fps_timer;
    double prev_time, cur_time;

    /* Basic state flags */
    _Bool initialized_flag;
    _Bool minimized_flag;
    _Bool active_flag;
    _Bool quitting_flag;

    /* Audio engine */
    GstElement *pipeline, *adder, *sink;
    GstElement *tone[TONE_COUNT], *noise[NOISE_COUNT], *wave[WAVE_COUNT];
};


#endif // ENGINE_PRIV_H_INCLUDED
