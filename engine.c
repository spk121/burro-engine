#include "engine.h"
#include <glib.h>
#include <math.h>
#include <gtk/gtk.h>

#include <glib.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
//#include <gst/gst.h>
#include "engine.h"
#include "eng_draw.h"
#include "eng_audio.h"

/* For the GUI */
static GtkWidget *window;
static GtkWidget *fixed;
static GtkWidget *main_screen;
static GtkWidget *sub_screen;

/* Global state */
static gboolean initialized_flag = FALSE;
static gboolean minimized_flag = FALSE;
static gboolean active_flag = FALSE;
static gboolean quitting_flag = FALSE;
#define REFRESH_RATE (1.0 / 30.0)

/* For frames-per-second calculation */
static int frame_count;
static GTimer *fps_timer;
static double prev_time, cur_time;

/* The main engine data store */
engine_t e;

cairo_surface_t *eng_main_surface;
cairo_surface_t *eng_sub_surface;
GMainLoop *main_loop;

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

    gtk_init(argc, argv);
    gst_init(argc, argv);

    /* Set up the frame count timer */
    fps_timer = g_timer_new();
    g_timer_start (fps_timer);
    frame_count = 0;
    prev_time = g_timer_elapsed (fps_timer, NULL);
    cur_time =  g_timer_elapsed (fps_timer, NULL);

    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_container_set_border_width (GTK_CONTAINER (window), 8);
    gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
    if (title && strlen(title) > 0)
        gtk_window_set_title (GTK_WINDOW (window), title);
    else
        gtk_window_set_title(GTK_WINDOW(window), "Project Burro Engine");

    gtk_widget_realize (window);

    fixed = gtk_fixed_new ();
    gtk_container_add (GTK_CONTAINER (window), fixed);

    sub_screen = gtk_drawing_area_new();
    gtk_widget_set_size_request (sub_screen,
                                 SUB_SCREEN_WIDTH_IN_PIXELS * SUB_SCREEN_MAGNIFICATION,
                                 SUB_SCREEN_HEIGHT_IN_PIXELS * SUB_SCREEN_MAGNIFICATION);

    gtk_fixed_put(GTK_FIXED(fixed), sub_screen, 0, 0);

    main_screen = gtk_drawing_area_new();
    gtk_widget_set_size_request(main_screen,
                                MAIN_SCREEN_WIDTH_IN_PIXELS * MAIN_SCREEN_MAGNIFICATION,
                                MAIN_SCREEN_HEIGHT_IN_PIXELS * MAIN_SCREEN_MAGNIFICATION);

    gtk_fixed_put(GTK_FIXED(fixed), main_screen, SUB_SCREEN_WIDTH_IN_PIXELS * SUB_SCREEN_MAGNIFICATION + 10, 0);

    g_signal_connect (G_OBJECT(window), "destroy", G_CALLBACK (destroy_cb), NULL);
    g_signal_connect (G_OBJECT (window), "key-press-event", G_CALLBACK (key_event_cb), NULL);
    g_signal_connect (G_OBJECT (window), "key-release-event", G_CALLBACK (key_event_cb), NULL);
    g_signal_connect (GTK_WIDGET(window), "window-state-event", (GCallback) window_state_event_cb, NULL);

    gtk_widget_show_all (window);

    e.brightness =  1.0;

    initialize_globals();
    initialize_video();
    initialize_audio();
    initialize_timers();
    initialize_io();

    initialized_flag = TRUE;
}


void eng_main()
{
    active_flag = TRUE;

    /* Set up the main loop.  I use glib because I need an idle function */
    {

        main_loop = g_main_loop_new (NULL, TRUE);

        /* All our game processing goes in the idle func */
        g_idle_add (idle_state_event_cb, NULL);

        /* What is this GDK voodoo?  Grabbed it from the gtk repo */
        gdk_threads_leave ();
        g_main_loop_run (main_loop);
        gdk_threads_enter ();
        gdk_flush ();
        g_main_loop_unref (main_loop);
    }
}

static void destroy_cb(GtkWidget* widget, gpointer dummy)
{
    quitting_flag = TRUE;
    g_main_loop_quit(main_loop);
}

static gboolean window_state_event_cb (GtkWidget *widget, GdkEvent *event, gpointer dummy)
{
    if (event->window_state.changed_mask & GDK_WINDOW_STATE_ICONIFIED)
    {
        if (event->window_state.new_window_state & GDK_WINDOW_STATE_ICONIFIED)
            minimized_flag = TRUE;
        else
            minimized_flag = FALSE;
    }
    return TRUE;
}

static gboolean key_event_cb (GtkWidget *widget, GdkEventKey *event, gpointer dummy)
{
    switch (gdk_keyval_to_upper(event->keyval))
    {
    case GDK_KEY_W:
        e.key_up = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_A:
        e.key_left = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_D:
        e.key_down = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_F:
        e.key_right = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_space:
        e.key_a = event->type == GDK_KEY_PRESS ? 1 : 0;
    default:
        return FALSE;
    }

    return FALSE;
}

static gboolean idle_state_event_cb (void *dummy)
{
    static double cur_tick = 0.0;
    static double last_update = 0.0;
    static double last_draw = 0.0;
    static gboolean run_full_speed = FALSE;

    if (quitting_flag)
        return FALSE;

    if (initialized_flag && !minimized_flag)
    {
        if (active_flag)
        {
            cur_tick = g_timer_elapsed (fps_timer, NULL);

            if (e.do_idle != NULL)
                e.do_idle (&e, cur_tick - last_update);
            last_update = cur_tick;

            /* FIXME: check ini to see if we're running full speed */
            if (run_full_speed || ((cur_tick - last_draw) > REFRESH_RATE))
            {
                if (e.do_before_draw_frame != NULL)
                    e.do_before_draw_frame (&e, cur_tick - last_update);
                paint ();
                audio_update ();
                last_draw = cur_tick;
            }
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

    if (quitting_flag)
        return;
    /* Have the video view draw the video model onto the screen */

    cr = gdk_cairo_create (gtk_widget_get_window (main_screen));

    cairo_set_antialias (cr, CAIRO_ANTIALIAS_NONE);
    cairo_scale(cr, 2.0, 2.0);
    cairo_set_source_surface (cr, main_screen_surface, 0, 0);
    cairo_paint (cr);
    cairo_destroy(cr);

    cr = gdk_cairo_create (gtk_widget_get_window (sub_screen));

    cairo_set_antialias (cr, CAIRO_ANTIALIAS_NONE);
    cairo_scale(cr, 1.0, 1.0);
    cairo_set_source_surface (cr, sub_screen_surface, 0, 0);
    cairo_paint (cr);
    cairo_destroy(cr);

    frame_count ++;
    if (frame_count % 100 == 0)
    {
        cur_time = g_timer_elapsed(fps_timer, NULL);
        printf("%f\n", 100.0 / (cur_time - prev_time));
        prev_time = cur_time;
    }
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
