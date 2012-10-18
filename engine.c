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

/* For the GUI */
static GtkWidget *m_window;
static GtkWidget *m_fixed;
static GtkWidget *m_main_screen;
static GtkWidget *m_sub_screen;

/* Global state */
static gboolean m_initialized_flag = FALSE;
static gboolean m_minimized_flag = FALSE;
static gboolean m_active_flag = FALSE;
static gboolean m_quitting_flag = FALSE;
#define REFRESH_RATE (1.0 / 30.0)

/* For frames-per-second calculation */
static int m_frame_count;
static GTimer *m_fps_timer;
static double m_prev_time, m_cur_time;

/* The main engine data store */
engine_t e;

cairo_surface_t *eng_main_surface;
cairo_surface_t *eng_sub_surface;

static gboolean window_state_event_cb (GtkWidget *widget, GdkEvent *event, gpointer dummy);
static gboolean idle_state_event_cb (void *dummy);
static gboolean key_event_cb (G_GNUC_UNUSED GtkWidget *widget, GdkEventKey *event, gpointer dummy);
static void paint (void);
static void present(void);
static void audio_update (void);
static void audio_pause (void);

void eng_init()
{

    g_log_set_handler ("Gtk", G_LOG_LEVEL_WARNING, (GLogFunc) gtk_false, NULL);
    gtk_init (0, NULL);
    //gst_init (0, NULL);
    g_log_set_handler (NULL, G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION, g_log_default_handler, NULL);
    /* Set up the frame count timer */
    m_fps_timer = g_timer_new();
    g_timer_start (m_fps_timer);
    m_frame_count = 0;
    m_prev_time = g_timer_elapsed (m_fps_timer, NULL);
    m_cur_time =  g_timer_elapsed (m_fps_timer, NULL);

    m_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_container_set_border_width (GTK_CONTAINER (m_window), 8);
    gtk_window_set_position(GTK_WINDOW(m_window), GTK_WIN_POS_CENTER);
    gtk_window_set_title (GTK_WINDOW (m_window), "burro");
    gtk_widget_realize (m_window);

    m_fixed = gtk_fixed_new ();
    gtk_container_add (GTK_CONTAINER (m_window), m_fixed);

    m_sub_screen = gtk_drawing_area_new();
    gtk_widget_set_size_request (m_sub_screen,
                                 SUB_SCREEN_WIDTH_IN_PIXELS * SUB_SCREEN_MAGNIFICATION,
                                 SUB_SCREEN_HEIGHT_IN_PIXELS * SUB_SCREEN_MAGNIFICATION);

    gtk_fixed_put(GTK_FIXED(m_fixed), m_sub_screen, 0, 0);

    m_main_screen = gtk_drawing_area_new();
    gtk_widget_set_size_request(m_main_screen,
                                MAIN_SCREEN_WIDTH_IN_PIXELS * MAIN_SCREEN_MAGNIFICATION,
                                MAIN_SCREEN_HEIGHT_IN_PIXELS * MAIN_SCREEN_MAGNIFICATION);

    gtk_fixed_put(GTK_FIXED(m_fixed), m_main_screen, SUB_SCREEN_WIDTH_IN_PIXELS * SUB_SCREEN_MAGNIFICATION + 10, 0);

    g_signal_connect (m_window, "destroy", G_CALLBACK (gtk_main_quit), NULL);
    g_signal_connect (G_OBJECT (m_window), "key-press-event", G_CALLBACK (key_event_cb), NULL);
    g_signal_connect (G_OBJECT (m_window), "key-release-event", G_CALLBACK (key_event_cb), NULL);
    g_signal_connect (GTK_WIDGET(m_window), "window-state-event", (GCallback) window_state_event_cb, NULL);

    gtk_widget_show_all (m_window);

    init_draw ();

    m_initialized_flag = TRUE;
}


void eng_main()
{
    m_active_flag = TRUE;

    /* Set up the main loop.  I use glib because I need an idle function */
    {
        GMainLoop *loop;
        loop = g_main_loop_new (NULL, TRUE);

        /* All our game processing goes in the idle func */
        g_idle_add (idle_state_event_cb, NULL);

        /* What is this GDK voodoo?  Grabbed it from the gtk repo */
        gdk_threads_leave ();
        g_main_loop_run (loop);
        gdk_threads_enter ();
        gdk_flush ();
        g_main_loop_unref (loop);
    }
}

static gboolean window_state_event_cb (GtkWidget *widget, GdkEvent *event, gpointer dummy)
{
    if (event->window_state.changed_mask & GDK_WINDOW_STATE_ICONIFIED)
    {
        if (event->window_state.new_window_state & GDK_WINDOW_STATE_ICONIFIED)
            m_minimized_flag = TRUE;
        else
            m_minimized_flag = FALSE;
    }
    return TRUE;
}

static gboolean key_event_cb (GtkWidget *widget, GdkEventKey *event, gpointer dummy)
{
    switch (event->keyval)
    {
#ifdef GTK3
    case GDK_KEY_p:
#else
    case GDK_p:
#endif
        printf("key pressed: %s\n", "p");
        break;
#ifdef GTK3
    case GDK_KEY_s:
#else
    case GDK_s:
#endif
        if (event->state & GDK_SHIFT_MASK)
        {
            printf("key pressed: %s\n", "shift + s");
        }
        else if (event->state & GDK_CONTROL_MASK)
        {
            printf("key pressed: %s\n", "ctrl + s");
        }
        else
        {
            printf("key pressed: %s\n", "s");
        }
        break;
#ifdef GTK3
    case GDK_KEY_m:
#else
    case GDK_m:
#endif
        if (event->state & GDK_SHIFT_MASK)
        {
            printf("key pressed: %s\n", "shift + m");
        }
        else if (event->state & GDK_CONTROL_MASK)
        {
            printf("key pressed: %s\n", "ctrl + m");
        }
        else
        {
            printf("key pressed: %s\n", "m");
        }
        break;

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

    if (m_quitting_flag)
        return FALSE;

    if (m_initialized_flag && !m_minimized_flag)
    {
        if (m_active_flag)
        {
            cur_tick = g_timer_elapsed (m_fps_timer, NULL);

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

    /* Have the video view draw the video model onto the screen */

    cr = gdk_cairo_create (gtk_widget_get_window (m_main_screen));

    cairo_set_antialias (cr, CAIRO_ANTIALIAS_NONE);
    cairo_scale(cr, 2.0, 2.0);
    cairo_set_source_surface (cr, eng_main_surface, 0, 0);
    cairo_paint (cr);
    cairo_destroy(cr);

    cr = gdk_cairo_create (gtk_widget_get_window (m_sub_screen));

    cairo_set_antialias (cr, CAIRO_ANTIALIAS_NONE);
    cairo_scale(cr, 1.0, 1.0);
    cairo_set_source_surface (cr, eng_sub_surface, 0, 0);
    cairo_paint (cr);
    cairo_destroy(cr);

    m_frame_count ++;
    if (m_frame_count % 100 == 0)
    {
        m_cur_time = g_timer_elapsed(m_fps_timer, NULL);
        printf("%f\n", 100.0 / (m_cur_time - m_prev_time));
        m_prev_time = m_cur_time;
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
