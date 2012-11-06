#include "engine.h"
#include <glib.h>
#include <math.h>
#include <gtk/gtk.h>

#include <glib.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
//#include <gst/gst.h>
#include "engine.h"
#include "eng_state.h"
#include "eng_video.h"
#include "eng_audio.h"
#include "eng_timers.h"
#include "eng_input.h"

#define UPDATE_RATE (1.0 / 60.0)
#define REFRESH_RATE (1.0 / 30.0)

/* The main engine data store */
engine_t e;

cairo_surface_t *eng_main_surface;
cairo_surface_t *eng_sub_surface;


static void destroy_cb(GtkWidget* widget, gpointer dummy);
static gboolean window_state_event_cb (GtkWidget *widget, GdkEvent *event, gpointer dummy);
static gboolean idle_state_event_cb (void *dummy);
static gboolean key_event_cb (G_GNUC_UNUSED GtkWidget *widget, GdkEventKey *event, gpointer dummy);
static void paint (void);
static void present(void);
static void audio_update (void);
static void audio_pause (void);

void engine_initialize(int *argc, char ***argv, char *title)
{
    g_warn_if_fail(title != NULL && strlen(title) > 0);
    g_warn_if_fail(argc != NULL && *argc > 0);
    g_warn_if_fail(argv != NULL);

    g_debug("Entering engine_initialize()");
    gtk_init(argc, argv);
    gst_init(argc, argv);

    memset(&e, 0, sizeof(engine_t));

    /* Set up the window */
    e.priv.window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_container_set_border_width (GTK_CONTAINER (e.priv.window), 8);
    gtk_window_set_position(GTK_WINDOW(e.priv.window), GTK_WIN_POS_CENTER);
    if (title && strlen(title) > 0)
        gtk_window_set_title (GTK_WINDOW (e.priv.window), title);
    else
        gtk_window_set_title(GTK_WINDOW(e.priv.window), "Project Burro Engine");

    gtk_widget_realize (e.priv.window);

    e.priv.fixed = gtk_fixed_new ();
    gtk_container_add (GTK_CONTAINER (e.priv.window), e.priv.fixed);

    e.priv.sub_screen = gtk_drawing_area_new();
    gtk_widget_set_size_request (e.priv.sub_screen,
                                 SUB_SCREEN_WIDTH_IN_PIXELS * SUB_SCREEN_MAGNIFICATION,
                                 SUB_SCREEN_HEIGHT_IN_PIXELS * SUB_SCREEN_MAGNIFICATION);

    gtk_fixed_put(GTK_FIXED(e.priv.fixed), e.priv.sub_screen, 0, 0);

    e.priv.main_screen = gtk_drawing_area_new();
    gtk_widget_set_size_request(e.priv.main_screen,
                                MAIN_SCREEN_WIDTH_IN_PIXELS * MAIN_SCREEN_MAGNIFICATION,
                                MAIN_SCREEN_HEIGHT_IN_PIXELS * MAIN_SCREEN_MAGNIFICATION);

    gtk_fixed_put(GTK_FIXED(e.priv.fixed), e.priv.main_screen, SUB_SCREEN_WIDTH_IN_PIXELS * SUB_SCREEN_MAGNIFICATION + 10, 0);

    g_signal_connect (G_OBJECT(e.priv.window), "destroy", G_CALLBACK (destroy_cb), NULL);
    g_signal_connect (G_OBJECT (e.priv.window), "key-press-event", G_CALLBACK (key_event_cb), NULL);
    g_signal_connect (G_OBJECT (e.priv.window), "key-release-event", G_CALLBACK (key_event_cb), NULL);
    g_signal_connect (GTK_WIDGET(e.priv.window), "window-state-event", (GCallback) window_state_event_cb, NULL);

    gtk_widget_show_all (e.priv.window);

    initialize_state();
    initialize_video();
    initialize_audio();
    initialize_timers();
    initialize_input();

    e.priv.initialized_flag = TRUE;
}


void engine_loop()
{
    g_debug("Entering engine_loop()");
    e.priv.active_flag = TRUE;

    /* Set up the main loop.  I use glib because I need an idle function */
    e.priv.main_loop = g_main_loop_new (NULL, TRUE);

    /* All our game processing goes in the idle func */
    g_idle_add (idle_state_event_cb, NULL);

    /* What is this GDK voodoo?  Grabbed it from the gtk repo */
    gdk_threads_leave ();
    g_main_loop_run (e.priv.main_loop);
    gdk_threads_enter ();
    gdk_flush ();
    g_main_loop_unref (e.priv.main_loop);
}

static void destroy_cb(GtkWidget* widget, gpointer dummy)
{
    e.priv.quitting_flag = TRUE;
    g_main_loop_quit(e.priv.main_loop);
}

static gboolean window_state_event_cb (GtkWidget *widget, GdkEvent *event, gpointer dummy)
{
    if (event->window_state.changed_mask & GDK_WINDOW_STATE_ICONIFIED)
    {
        if (event->window_state.new_window_state & GDK_WINDOW_STATE_ICONIFIED)
            e.priv.minimized_flag = TRUE;
        else
            e.priv.minimized_flag = FALSE;
    }
    return TRUE;
}

static gboolean key_event_cb (GtkWidget *widget, GdkEventKey *event, gpointer dummy)
{
    switch (gdk_keyval_to_upper(event->keyval))
    {
    case GDK_KEY_a:
    case GDK_KEY_A:
        e.key_a = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_b:
    case GDK_KEY_B:
        e.key_b = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_x:
    case GDK_KEY_X:
        e.key_x = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_y:
    case GDK_KEY_Y:
        e.key_y = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Up:
    case GDK_KEY_KP_Up:
        e.key_up = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Down:
    case GDK_KEY_KP_Down:
        e.key_down = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Left:
    case GDK_KEY_KP_Left:
        e.key_left = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Right:
    case GDK_KEY_KP_Right:
        e.key_right = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Start:
        e.key_start = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Select:
    case GDK_KEY_SelectButton:
    case GDK_KEY_KP_Tab:
    case GDK_KEY_Tab:
        e.key_select = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_KP_Enter:
    case GDK_KEY_space:
        e.key_start = event->type == GDK_KEY_PRESS ? 1 : 0;
    default:
        return FALSE;
    }

    return FALSE;
}

static gboolean idle_state_event_cb (void *dummy)
{
    double cur_time;

    if (e.priv.quitting_flag)
        return FALSE;

    cur_time = g_timer_elapsed (e.priv.timer, NULL);

    if (e.priv.initialized_flag && !e.priv.minimized_flag)
    {
        if (e.priv.active_flag)
        {
            if (e.priv.run_full_speed_flag || ((cur_time - e.priv.before_update_time) > UPDATE_RATE))
            {
                if (e.do_idle != NULL)
                    e.do_idle (cur_time - e.priv.before_update_time);

                e.priv.update_count ++;
                e.priv.before_update_time = cur_time;
                e.priv.after_update_time = g_timer_elapsed (e.priv.timer, NULL);

                /* FIXME: check ini to see if we're running full speed */
                if (e.priv.run_full_speed_flag || ((cur_time - e.priv.before_draw_time) > REFRESH_RATE))
                {
                    paint ();
                    audio_update ();
                    if (e.do_after_draw_frame != NULL)
                        e.do_after_draw_frame (e.priv.before_draw_time - e.priv.after_draw_time);

                    e.priv.draw_count ++;
                    e.priv.before_draw_time = cur_time;
                    e.priv.after_draw_time = g_timer_elapsed (e.priv.timer, NULL);

                    if (e.priv.draw_count % 100 == 0)
                    {
                        g_debug("FPS: %f", 100.0 / (e.priv.after_draw_time - e.priv.hundred_frames_draw_time));
                        e.priv.hundred_frames_draw_time = e.priv.after_draw_time;
                    }
                }
            }

            if (!e.priv.run_full_speed_flag)
                g_usleep (2);
        }
        else
        {
            audio_pause ();
            // Figure out a way to sleep until the next gtk event comes in
            g_usleep (2);
        }
    }

    return TRUE;
}

static void paint ()
{
    /* Have the engine render the backgrounds and objects to a bitmap */
    draw ();

    /* Have GTK draw the bitmap to the screen */
    present ();
}

static void present()
{
    cairo_t *cr;

    if (e.priv.quitting_flag)
        return;
    /* Have the video view draw the video model onto the screen */

    cr = gdk_cairo_create (gtk_widget_get_window (e.priv.main_screen));

    cairo_set_antialias (cr, CAIRO_ANTIALIAS_NONE);
    cairo_scale(cr, 2.0, 2.0);
    cairo_set_source_surface (cr, e.priv.main_screen_surface, 0, 0);
    // cairo_surface_write_to_png(e.priv.main_screen_surface, "burro_main_screen_present.png");
    cairo_paint (cr);
    cairo_destroy(cr);

    cr = gdk_cairo_create (gtk_widget_get_window (e.priv.sub_screen));

    cairo_set_antialias (cr, CAIRO_ANTIALIAS_NONE);
    cairo_scale(cr, 1.0, 1.0);
    cairo_set_source_surface (cr, e.priv.sub_screen_surface, 0, 0);
    // cairo_surface_write_to_png(e.priv.sub_screen_surface, "burro_sub_screen_present.png");
    cairo_paint (cr);
    cairo_destroy(cr);
}

static void audio_update ()
{
    /* update each of the 3 tone channels */
}

static void audio_pause ()
{
    /* update each of the 3 tone channels */
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
