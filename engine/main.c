#include "x.h"
#include "y.h"

static bool fullspeed = false;
static uint32_t seed = (uint32_t) -1;
static GtkWidget *mainwin = NULL;

static GOptionEntry entries[] =
{
    { "full-speed", 0, 0, G_OPTION_ARG_NONE, &fullspeed, "Run at maximum frame rate", NULL },
    { "seed", 0, 0, G_OPTION_ARG_INT, &seed, "Random number seed", "positive integer" },
    { NULL }
};

void
log_handler (const char *log_domain,
             GLogLevelFlags log_level,
             const char *message,
             void *user_data)
{
    fprintf (stdout, "%s\n", message);
    fflush (stdout);
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
    g_log_set_handler (NULL, debug_flags, log_handler, NULL);
    g_log_set_handler ("GLib", debug_flags, log_handler, NULL);
    g_log_set_handler ("Gtk", debug_flags, log_handler, NULL);

    /* Initialize memory cache */
    /* Create the window */
    mainwin = eng_initialize();
    xgtk_window_set_application (GTK_WINDOW (mainwin), app);
    gtk_widget_show_all (mainwin);

    /* Initialize the audio system */

    /* Load player's game options and saved game files */
    
    /* Create drawing surface */
    draw_initialize();

    /* All other stuff */
    console_reset ();
    console_test_pattern ();
    lineedit_initialize ();
    // init_guile_guile_procedures();
    init_lisp();
    // G_console();
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
