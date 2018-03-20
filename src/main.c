/*  main.c

    Copyright (C) 2018   Michael L. Gran
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

#include "x.h"
#include "y.h"
#include "r/burro.resource.h"

static GtkWidget *mainwin = NULL;

void
stdout_log_handler (const char *log_domain,
                    GLogLevelFlags log_level,
                    const char *message,
                    void *user_data)
{
    fprintf (stdout, "%s\n", message);
    fflush (stdout);
}

static void
console_log_handler (const char *log_domain,
                     GLogLevelFlags log_level,
                     const char *message,
                     void *user_data)
{
    if (console_is_visible()
        && (log_level == G_LOG_LEVEL_ERROR
            || log_level == G_LOG_LEVEL_CRITICAL
            || log_level == G_LOG_LEVEL_WARNING)) {
        console_write_latin1_string (message);
        console_move_to_column(0); 
        console_move_down(1);
    }
    else {
        fprintf (stdout, "%s\n", message);
        fflush (stdout);
    }
}

static void
stdio_log_handler (const gchar   *log_domain,
             GLogLevelFlags log_level,
             const gchar   *message,
             gpointer       user_data)
{
    g_log_default_handler (log_domain, log_level, message, user_data);

//    if (log_level != G_LOG_LEVEL_DEBUG && log_level != G_LOG_LEVEL_INFO)
//        g_on_error_query ("BURRO");
}

static void
main_initialize (GtkApplication *app, GFile *file)
{
    /* Check system resources */
    /* Check CPU speed */
    /* Estimate VRAM */
    /* Initialize random number generator */
    rand_init ();

    /* Load debugging options */
    const GLogLevelFlags debug_flags = (GLogLevelFlags) (G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION);
    g_log_set_handler (NULL, debug_flags, stdio_log_handler, NULL);
    g_log_set_handler ("GLib", debug_flags, stdio_log_handler, NULL);
    g_log_set_handler ("Gtk", debug_flags, stdio_log_handler, NULL);

    /* Initialize memory cache */
    vram_init ();
    backdrop_set_color (0x00000000);
    bg_init ();
    bg_assign_memory (BG_MAIN_0, MATRIX_256x256, VRAM_C);
    bg_assign_memory (BG_MAIN_1, MATRIX_256x256, VRAM_D);
    bg_assign_memory (BG_MAIN_2, MATRIX_128x128, VRAM_E);
    bg_assign_memory (BG_MAIN_3, MATRIX_32x32, VRAM_F);

    /* Create the window */
    ecma48_init ();
    mainwin = eng_initialize();
    xgtk_window_set_application (GTK_WINDOW (mainwin), app);
    gtk_widget_show_all (mainwin);

    /* Load player's game options and saved game files */

    /* Create drawing surface */
    draw_initialize();

    /* All other stuff */
    console_reset ();
    lineedit_initialize ();
    // init_guile_guile_procedures();
    // G_console();
    loop_initialize();

    /* Initialize the audio system */
    // audio_model_initialize ();
    pulse_initialize_audio_step_1 ();

    /* Register compiled-in resources (). */
    burro_get_resource();
    
    if (file)
    {
        char *scheme_file = g_file_get_path (file);
        init_lisp(scheme_file);
        g_free (scheme_file);
    }
    else
        init_lisp (NULL);
    
    loop ();
}

static void
app_activate_cb (GtkApplication *app)
{
    GList *list;

    list = xgtk_application_get_windows (app);

    if (list)
        gtk_window_present (GTK_WINDOW (list->data));
    else
        main_initialize (app, NULL);
}

static void
app_open_cb (GtkApplication *app,
             GFile **files,
             gint n_files,
             const char *hint)
{
    main_initialize (app, files[0]);
}
                    
int main (int argc, char **argv)
{
    GOptionContext *context;
    GtkApplication *app;
    int status;
    GError *error = NULL;

    // Use debug version of Scheme engine.
    // FIXME: found this solution by diving Guile source.
    // There is probably a more legit method.
    extern void scm_c_set_default_vm_engine_x (int x);
    scm_c_set_default_vm_engine_x (1);
    xscm_init_guile ();

    app = gtk_application_new ("com.lonelycactus.burroengine",
                               G_APPLICATION_NON_UNIQUE
                               | G_APPLICATION_HANDLES_OPEN);

    g_signal_connect (app, "activate", G_CALLBACK (app_activate_cb), NULL);
    g_signal_connect (app, "open", G_CALLBACK (app_open_cb), NULL);
    g_application_set_inactivity_timeout (G_APPLICATION(app), 1000*60*60);
    status = xg_application_run (G_APPLICATION (app), argc, argv);

    g_object_unref (app);

    return status;
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
