#include "x.h"
#include "y.h"

static bool fullspeed = false;
static uint32_t seed = (uint32_t) -1;
static GtkWidget *mainwin = NULL;
static char *scheme_file = NULL;

static GOptionEntry entries[] =
{
    { "full-speed", 0, 0, G_OPTION_ARG_NONE, &fullspeed, "Run at maximum frame rate", NULL },
    { "seed", 0, 0, G_OPTION_ARG_INT, &seed, "Random number seed", "integer" },
    { "load", 'L', 0, G_OPTION_ARG_FILENAME, &scheme_file, "the main scheme file to run", "filename"},
    { NULL }
};

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

  if (log_level != G_LOG_LEVEL_DEBUG && log_level != G_LOG_LEVEL_INFO)
      g_on_error_query ("BURRO");
}

static void
initialize (GtkApplication *app)
{
    /* Check system resources */
    /* Check CPU speed */
    /* Estimate VRAM */
    /* Initialize random number generator */
    if (seed == -1)
        rand_init ();
    else
        rand_init_with_seed (seed);

    /* Load debugging options */
    const GLogLevelFlags debug_flags = (GLogLevelFlags) (G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION);
    g_log_set_handler (NULL, debug_flags, stdio_log_handler, NULL);
    g_log_set_handler ("GLib", debug_flags, stdio_log_handler, NULL);
    g_log_set_handler ("Gtk", debug_flags, stdio_log_handler, NULL);

    /* Initialize memory cache */
    vram_init ();
    backdrop_set_color (BACKDROP_MAIN, 0x00000000);
    backdrop_set_color (BACKDROP_SUB, 0x00000000);
    sheet_init ();
    sheet_assign_memory (SHEET_MAIN_BG, MATRIX_512x512, VRAM_0);
    sheet_assign_memory (SHEET_SUB_BG, MATRIX_512x512, VRAM_1);
    sheet_assign_memory (SHEET_MAIN_OBJ, MATRIX_256x256, VRAM_A);
    sheet_assign_memory (SHEET_SUB_OBJ, MATRIX_256x256, VRAM_B);
    bg_init ();
    bg_assign_memory (BG_MAIN_0, MATRIX_256x256, VRAM_C);
    bg_assign_memory (BG_MAIN_1, MATRIX_256x256, VRAM_D);
    bg_assign_memory (BG_MAIN_2, MATRIX_128x128, VRAM_E);
    bg_assign_memory (BG_MAIN_3, MATRIX_32x32, VRAM_F);
    bg_assign_memory (BG_SUB_0, MATRIX_32x32, VRAM_G);
    bg_assign_memory (BG_SUB_1, MATRIX_32x32, VRAM_H);
    bg_assign_memory (BG_SUB_2, MATRIX_32x32, VRAM_I);
    bg_assign_memory (BG_SUB_3, MATRIX_32x32, VRAM_J);

    /* Create the window */
    init_lisp(scheme_file);
    ecma48_init ();
    mainwin = eng_initialize();
    xgtk_window_set_application (GTK_WINDOW (mainwin), app);
    gtk_widget_show_all (mainwin);


    /* Load player's game options and saved game files */
    
    /* Create drawing surface */
#if 0
    for (int i = 0; i < 8; i ++) {
        bg_init(i, BG_TYPE_NONE, MATRIX_16x16, VRAM_I);
        bg_hide(i);
    }
    for (int i = 0; i < 256; i ++) {
        obj_init(i, 0, 0, OBJ_SIZE_16x16, 0.0, 0.0, false, false);
        obj_hide(i);
    }
#endif
    draw_initialize();

    /* All other stuff */
    console_reset ();
    lineedit_initialize ();
    // init_guile_guile_procedures();
    // G_console();
    loop_initialize();
    
    /* Initialize the audio system */
    audio_model_initialize ();
    pulse_initialize_audio_step_1 ();
    loop ();
    
}

static void
activate (GtkApplication *app)
{
    GList *list;

    list = xgtk_application_get_windows (app);

    if (list)
        gtk_window_present (GTK_WINDOW (list->data));
    else
        initialize (app);
}

int main (int argc, char **argv)
{
    GOptionContext *context;
    GtkApplication *app;
    int status;
    GError *error = NULL;

    xscm_init_guile ();

    context = g_option_context_new ("- game options");
    g_option_context_add_main_entries (context, entries, NULL);
    g_option_context_add_group (context, gtk_get_option_group (true));

    if (!g_option_context_parse (context, &argc, &argv, &error))
    {
        printf ("option parsing failed: %s\n", error->message);
        exit (1);
    }

    xgtk_init_check (&argc, &argv);

    app = gtk_application_new ("com.lonelycactus.projectburro",
                               G_APPLICATION_FLAGS_NONE);

    g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);
    g_application_set_inactivity_timeout (G_APPLICATION(app), 100000000);
    g_application_set_flags (G_APPLICATION(app), G_APPLICATION_NON_UNIQUE);
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
