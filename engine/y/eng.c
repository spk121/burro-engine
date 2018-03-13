#include "../x/xgdk.h"
#include "../x/xglib.h"
#include "../x/xgtk.h"
#include "console.h"
#include "draw.h"
#include "eng.h"
#include "ecma48.h"
#include "guile.h"
#include "lineedit.h"
#include "loop.h"
#include "repl.h"
#include <glib.h>
#include <gtk/gtk.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <sys/socket.h>

GtkWidget *window;
GtkWidget *fixed;
GtkWidget *main_screen;
gulong destroy_signal_id;
gulong key_press_event_signal_id;
gulong key_release_event_signal_id;
gulong window_state_event_signal_id;
gboolean blank_flag = FALSE;
gboolean colorswap_flag = FALSE;
gdouble brightness = 1.0;

int repl_socket = -1;
struct sockaddr_in repl_sock_addr;
GIOChannel *repl_io_channel;

static GMutex keymutex;
static int key_a, key_b, key_x, key_y;
static int key_start, key_select;
static int key_up, key_down, key_left, key_right;

static void destroy_cb(GtkWidget* widget, gpointer dummy);
static gboolean key_event_cb (GtkWidget *widget, GdkEventKey *event, gpointer dummy);
static bool key_event_console (unsigned keysym, unsigned state);
static gboolean
main_draw_cb (GtkWidget *widget,
         cairo_t *cr,
         gpointer data);

gboolean
eng_is_blank ()
{
    return blank_flag;
}

void
eng_blank ()
{
    blank_flag = TRUE;
}

void
eng_unblank ()
{
    blank_flag = FALSE;
}

gboolean
eng_is_colorswap ()
{
    return colorswap_flag;
}

void
eng_colorswap ()
{
    colorswap_flag = TRUE;
}

void
eng_uncolorswap ()
{
    colorswap_flag = FALSE;
}

double
eng_get_brightness ()
{
    return brightness;
}

void
eng_set_brightness (gdouble b)
{
    brightness = b;
}

static int
repl_input_cb (GIOChannel *source, GIOChannel condition, void *unused)
{
    char buf[256];
    size_t bytes_read;
    g_io_channel_read_chars (source, buf, 255, &bytes_read, NULL);
    if (bytes_read > 0)
        ecma48_execute (buf, bytes_read);
    return 0;
}

GtkWidget *eng_initialize ()
{
    window = xgtk_window_new (GTK_WINDOW_TOPLEVEL);
    xgtk_container_set_border_width (GTK_CONTAINER (window), 8);
    xgtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
    xgtk_window_set_title(GTK_WINDOW(window), "Project Burro Engine");

    /* Set GNOME properties that PulseAudio likes to have */
    xg_set_application_name("Burro Engine");
    xgtk_window_set_default_icon_name("applications-games");
    xg_setenv("PULSE_PROP_media.role", "game", TRUE);

    xgtk_widget_realize (window);

    fixed = xgtk_fixed_new ();
    xgtk_container_add (GTK_CONTAINER (window), fixed);

    main_screen = xgtk_drawing_area_new();
    xgtk_widget_set_size_request(main_screen,
                                 MAIN_SCREEN_WIDTH * MAIN_SCREEN_MAGNIFICATION,
                                 MAIN_SCREEN_HEIGHT * MAIN_SCREEN_MAGNIFICATION);

    xgtk_fixed_put(GTK_FIXED(fixed),
                   main_screen,
                   0, 0);

    g_mutex_init(&keymutex);

    destroy_signal_id =
        xg_signal_connect (G_OBJECT(window), "destroy", G_CALLBACK (destroy_cb), NULL);
    key_press_event_signal_id =
        xg_signal_connect (G_OBJECT (window), "key-press-event", G_CALLBACK (key_event_cb), NULL);
    key_release_event_signal_id =
        xg_signal_connect (G_OBJECT (window), "key-release-event", G_CALLBACK (key_event_cb), NULL);
    xg_signal_connect (G_OBJECT (main_screen), "draw", G_CALLBACK (main_draw_cb), NULL);

    /* window_state_event_signal_id =  */
    /*     xg_signal_connect (GTK_WIDGET(window), "window-state-event", G_CALLBACK (window_state_event_cb), NULL); */

#if 0
    repl_init ();
    repl_socket = socket (AF_INET, SOCK_DGRAM, 0);
    memset (&repl_sock_addr, 0, sizeof (repl_sock_addr));
    repl_sock_addr.sin_family = AF_INET;
    repl_sock_addr.sin_port = htons(37147);
    inet_pton(AF_INET, "127.0.0.1", &repl_sock_addr.sin_addr);
    connect (repl_socket, (struct sockaddr *) &repl_sock_addr, sizeof(repl_sock_addr));
    repl_io_channel = g_io_channel_unix_new (repl_socket);
    g_io_add_watch (repl_io_channel, G_IO_IN | G_IO_PRI, repl_input_cb, NULL);
    g_io_channel_set_encoding (repl_io_channel, NULL, NULL);
#endif

    return window;
}


static void destroy_cb (GtkWidget* widget, gpointer dummy)
{
    loop_quit ();
}

static gboolean
main_draw_cb (GtkWidget *widget,
         cairo_t *cr,
         gpointer data)
{
    xcairo_set_source_surface (cr, draw_get_main_screen_surface (), 0, 0);
    xcairo_paint (cr);
    return FALSE;
}

void eng_present()
{
    cairo_t *cr;

    /* Have the video view draw the video model onto the screen */
#if 0
    cr = xgdk_cairo_create (xgtk_widget_get_window (main_screen));
    // xcairo_set_antialias (cr, CAIRO_ANTIALIAS_NONE);
    // xcairo_scale(cr, MAIN_SCREEN_MAGNIFICATION, MAIN_SCREEN_MAGNIFICATION);
    xcairo_set_source_surface (cr, draw_get_main_screen_surface (), 0, 0);
    cairo_surface_write_to_png(draw_get_main_screen_surface(), "burro_main_screen_present.png");
    xcairo_paint (cr);
    xcairo_destroy(cr);
#else
    #if 0
    cairo_rectangle_int_t rect = {0, 0, MAIN_SCREEN_WIDTH, MAIN_SCREEN_HEIGHT};
    cairo_region_t *region = cairo_region_create_rectangle (&rect);
    GdkWindow *window = xgtk_widget_get_window (main_screen);
    GdkDrawingContext *gdc = gdk_window_begin_draw_frame(window, region);
    g_object_ref (G_OBJECT(gdc));
    cr = gdk_drawing_context_get_cairo_context (gdc);
    // xcairo_set_antialias (cr, CAIRO_ANTIALIAS_NONE);
    // xcairo_scale(cr, MAIN_SCREEN_MAGNIFICATION, MAIN_SCREEN_MAGNIFICATION);
    xcairo_set_source_surface (cr, draw_get_main_screen_surface (), 0, 0);
    cairo_surface_write_to_png(draw_get_main_screen_surface(), "burro_main_screen_present.png");
    xcairo_paint (cr);
    xcairo_destroy(cr);
    gdk_window_end_draw_frame (window, gdc);
    gdk_window_invalidate_region (window, region, TRUE);
    cairo_region_destroy (region);
    gtk_widget_show (main_screen);
#endif
#endif
    #if 0
#endif
    gdk_window_invalidate_rect (xgtk_widget_get_window (main_screen), NULL, FALSE);
}

static gboolean key_event_cb (GtkWidget *widget, GdkEventKey *event, gpointer dummy)
{
    if (console_is_visible () && (event->type == GDK_KEY_PRESS))
        return key_event_console (event->keyval, event->state);

    g_mutex_lock(&keymutex);

    switch (gdk_keyval_to_upper(event->keyval))
    {
    case GDK_KEY_A:
        key_a = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_B:
        key_b = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_X:
        key_x = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Y:
        key_y = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Up:
    case GDK_KEY_KP_Up:
        key_up = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Down:
    case GDK_KEY_KP_Down:
        key_down = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Left:
    case GDK_KEY_KP_Left:
        key_left = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Right:
    case GDK_KEY_KP_Right:
        key_right = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Start:
    case GDK_KEY_KP_Enter:
    case GDK_KEY_space:
        key_start = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    case GDK_KEY_Select:
    case GDK_KEY_SelectButton:
    case GDK_KEY_KP_Tab:
    case GDK_KEY_Tab:
        key_select = event->type == GDK_KEY_PRESS ? 1 : 0;
        break;
    default:
        break;
    }

    g_mutex_unlock(&keymutex);

    return TRUE;
}

static bool
key_event_console (unsigned keysym, unsigned state)
{
    // Here we process non-textual keys
    if (keysym == GDK_KEY_BackSpace)
        lineedit_backspace();
    else if (keysym == GDK_KEY_Tab)
        lineedit_autocomplete();
    else if ((state & GDK_CONTROL_MASK))
    {
        if ((keysym == GDK_KEY_A) || (keysym == GDK_KEY_a))
            lineedit_move_home();
        if (keysym == GDK_KEY_B || (keysym == GDK_KEY_b))
            lineedit_move_left();
        if (keysym == GDK_KEY_C || (keysym == GDK_KEY_c))
            lineedit_ctrl_c();
        if (keysym == GDK_KEY_D || (keysym == GDK_KEY_d))
            lineedit_delete_or_eof();
        if (keysym == GDK_KEY_E || (keysym == GDK_KEY_e))
            lineedit_move_end();
        if (keysym == GDK_KEY_F || (keysym == GDK_KEY_f))
            lineedit_move_right();
        if (keysym == GDK_KEY_G || (keysym == GDK_KEY_g))
            lineedit_backspace();
        if (keysym == GDK_KEY_H || (keysym == GDK_KEY_h))
            lineedit_delete_to_end();
        if (keysym == GDK_KEY_L || (keysym == GDK_KEY_l))
            lineedit_clear_screen();
        if (keysym == GDK_KEY_N || (keysym == GDK_KEY_n))
            lineedit_history_next();
        if (keysym == GDK_KEY_P || (keysym == GDK_KEY_p))
            lineedit_history_prev();
        if (keysym == GDK_KEY_T || (keysym == GDK_KEY_t))
            lineedit_swap_chars();
        if (keysym == GDK_KEY_Y || (keysym == GDK_KEY_y))
            lineedit_delete_line();
        if (keysym == GDK_KEY_W || (keysym == GDK_KEY_w))
            lineedit_delete_word_prev();
    }
    else if (keysym == GDK_KEY_Delete)
        lineedit_delete();
    else if (keysym == GDK_KEY_Down)
        lineedit_history_next();
    else if (keysym == GDK_KEY_End)
        lineedit_move_end();
    else if (keysym == GDK_KEY_Home)
        lineedit_move_home();
    else if ((keysym == GDK_KEY_Insert) || (keysym == GDK_KEY_KP_Insert))
        lineedit_toggle_insert_mode();
    else if (keysym == GDK_KEY_Left)
        lineedit_move_left();
    else if (keysym == GDK_KEY_Right)
        lineedit_move_right();
    else if (keysym == GDK_KEY_Up)
        lineedit_history_prev();
    else if (keysym == GDK_KEY_Tab)
        ;
    else if (keysym == GDK_KEY_Clear)
        ;
    else if (keysym == GDK_KEY_Pause)
        ;
    else if (keysym == GDK_KEY_Delete)
    {
    }
    else if (keysym == GDK_KEY_Return) {
        // End this lineedit session
        // Act on the string
        // Maybe add the string to the history
        lineedit_stop();
        console_move_to_column(0);
        console_move_down(1);

        char *script = lineedit_get_text();
        if (strlen(script) > 0) {
            // Call script callback with the current string
#if 1
            SCM ret = guile_c_eval_string_safe (script);
            g_free (script);
            char *text = guile_any_to_c_string (ret);
            if (text) {
                console_write_utf8_string(text);
                console_move_to_column(0);
                console_move_down(1);
                g_free(text);
            }
#endif
#if 0
            g_io_channel_write_chars (repl_io_channel, script, -1, NULL, NULL);
            g_io_channel_write_chars (repl_io_channel, "\n", 1, NULL, NULL);
#endif
            lineedit_start(linenoiseLineBuf, LINENOISE_MAX_LINE, L"->");
        }
    }
    else if (keysym == GDK_KEY_grave) {
        console_hide();
        return TRUE;
    }
    else if (keysym >= GDK_KEY_space && keysym <= GDK_KEY_ydiaeresis)
    {
        wchar_t input[2];
        input[0] = keysym;
        input[1] = L'\0';
        // if (autocomplete_flag)
        //     lineedit_autocomplete_text_input(input);
        // else
        lineedit_text_input(input);
    }
    return true;
}

unsigned int
eng_get_keyinput()
{
    // g_mutex_lock(&keymutex);

    return key_a | (key_b << 1) | (key_select << 2) | (key_start << 3)
        | (key_right << 4) | (key_left << 5) | (key_up << 6) | (key_down << 7)
        | (key_x << 8) | (key_y << 9);

    // g_mutex_unlock(&keymutex);

}

////////////////////////////////////////////////////////////////

SCM_DEFINE (G_eng_is_blank, "eng-blank?", 0, 0, 0, (void), "")
{
    return scm_from_bool (eng_is_blank());
}

SCM_DEFINE (G_eng_blank, "eng-blank", 0, 0, 0, (void), "")
{
    eng_blank ();
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_eng_unblank, "eng-unblank", 0, 0, 0, (void), "")
{
    eng_unblank ();
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_eng_colorswap_p, "eng-colorswap?", 0, 0, 0, (void), "")
{
    return scm_from_bool (eng_is_colorswap ());
}

SCM_DEFINE (G_eng_colorswap, "eng-colorswap", 0, 0, 0, (void), "")
{
    eng_colorswap ();
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_eng_uncolorswap, "eng-uncolorswap", 0, 0, 0, (void), "")
{
    eng_uncolorswap ();
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_eng_get_brightness, "eng-get-brightness", 0, 0, 0, (void), "")
{
    return scm_from_double (eng_get_brightness ());
}

SCM_DEFINE (G_eng_set_brightness, "eng-set-brightness", 1, 0, 0, (SCM brightness), "")
{
    eng_set_brightness (scm_to_double (brightness));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_eng_get_keyinput, "eng-get-keyinput", 0, 0, 0, (void), "")
{
    return scm_from_int (eng_get_keyinput ());
}

void
eng_init_guile_procedures ()
{
#include "eng.x"
    scm_c_export ("screen-blank?", "screen-blank", "screen-unblank",
                  /*"eng-colorswap?", "eng-colorswap", "eng-uncolorswap",
                    "eng-get-brightness", "eng-set-brightness", */
                  "eng-get-keyinput",
                  NULL);
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
