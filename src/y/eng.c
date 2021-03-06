/*  eng.c

    Copyright (C) 2013, 2014, 2018   Michael L. Gran
    This file is part of Burro Engine

    Burro Engine is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Burro Engine is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Burro Engine.  If not, see <http://www.gnu.org/licenses/>.
*/
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
gulong button_press_event_signal_id;
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

static guint32 mouse_move_time = 0;
static gdouble mouse_move_x = -1;
static gdouble mouse_move_y = -1;
static guint mouse_move_state = 0;

static guint32 button_press_time = 0;
static gdouble button_press_x = -1;
static gdouble button_press_y = -1;



static void destroy_cb(GtkWidget* widget, gpointer dummy);
static gboolean key_event_cb (GtkWidget *widget, GdkEventKey *event, gpointer dummy);
static gboolean button_press_event_cb (GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean motion_notify_event_cb (GtkWidget *widget, GdkEventMotion *event, gpointer user_data);
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
    gtk_widget_set_events (main_screen, GDK_EXPOSURE_MASK
                           | GDK_LEAVE_NOTIFY_MASK
                           | GDK_BUTTON_PRESS_MASK
                           | GDK_POINTER_MOTION_MASK
                           | GDK_POINTER_MOTION_HINT_MASK);
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
    button_press_event_signal_id =
        xg_signal_connect (G_OBJECT (main_screen), "button-press-event", G_CALLBACK (button_press_event_cb), NULL);
    xg_signal_connect (G_OBJECT (main_screen), "motion-notify-event", G_CALLBACK (motion_notify_event_cb), NULL);
    xg_signal_connect (G_OBJECT (main_screen), "draw", G_CALLBACK (main_draw_cb), NULL);

    /* window_state_event_signal_id =  */
    /*     xg_signal_connect (GTK_WIDGET(window), "window-state-event", G_CALLBACK (window_state_event_cb), NULL); */

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
    gdk_window_invalidate_rect (xgtk_widget_get_window (main_screen), NULL, FALSE);
}

static gboolean key_event_cb (GtkWidget *widget, GdkEventKey *event, gpointer dummy)
{
    if (console_is_visible () && console_is_repl ()
        && (event->type == GDK_KEY_PRESS))
        return key_event_console (event->keyval, event->state);

    g_mutex_lock(&keymutex);

    switch (gdk_keyval_to_upper(event->keyval))
    {
    case GDK_KEY_asciitilde:
    case GDK_KEY_dead_tilde:
    case GDK_KEY_dead_belowtilde:
        if (event->type == GDK_KEY_PRESS)
        {
            if (console_is_visible ())
                console_hide ();
            else
                console_show ();
        }
        break;
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

            //lineedit_start(linenoiseLineBuf, LINENOISE_MAX_LINE, L"->");
            lineedit_start();
        }
    }
#if 0
    else if (keysym == GDK_KEY_grave) {
        console_hide();
        return TRUE;
    }
#endif
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

static gboolean
button_press_event_cb (GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    if (event->button == 1)
    {
        button_press_time = event->time;
        button_press_x = event->x;
        button_press_y = event->y;
    }
    return true;
}

static gboolean
motion_notify_event_cb (GtkWidget *widget, GdkEventMotion *event, gpointer user_data)
{
    mouse_move_time = event->time;
    mouse_move_x = event->x;
    mouse_move_y = event->y;
    mouse_move_state = event->state;
    return true;
}

void
eng_emit_shutdown (void)
{
    // extern GtkApplication *app;
    loop_quit ();
    // g_application_quit (app);
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

SCM_DEFINE (G_eng_get_mouse_move, "eng-get-mouse-move", 0, 0, 0, (void), "\
Return the time, x, and y of the last mouse movement.")
{
    return scm_list_3 (scm_from_double (0.001 * mouse_move_time),
                       scm_from_double (mouse_move_x),
                       scm_from_double (mouse_move_y));
}

SCM_DEFINE (G_eng_get_button_press, "eng-get-button-press", 0, 0, 0, (void), "\
Return the time, x, and y of the last mouse click of button 1.")
{
    if (!button_press_time)
        return SCM_BOOL_F;

    SCM ret =  scm_list_3 (scm_from_double (0.001 * button_press_time),
                       scm_from_double (button_press_x),
                       scm_from_double (button_press_y));

    button_press_time = 0;
    button_press_x = -1;
    button_press_y = -1;
    return ret;
}

SCM_DEFINE (G_window_height, "window-height", 0, 0, 0, (void), "\
Return the height of the main window.")
{
    return scm_from_int (MAIN_SCREEN_HEIGHT);
}

SCM_DEFINE (G_window_width, "window-width", 0, 0, 0, (void), "\
Return the width of the main window.")
{
    return scm_from_int (MAIN_SCREEN_WIDTH);
}

void
eng_init_guile_procedures ()
{
#include "eng.x"
    scm_c_export ("screen-blank?", "screen-blank", "screen-unblank",
                  /*"eng-colorswap?", "eng-colorswap", "eng-uncolorswap",
                    "eng-get-brightness", "eng-set-brightness", */
                  "eng-get-keyinput", "eng-get-mouse-move", "eng-get-button-press",
                  "window-height", "window-width",
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
