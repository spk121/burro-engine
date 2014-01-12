#include "r/resource.h"
#include "x/xguile.h"
#include "x/xgtk.h"
#include "y/draw.h"
#include "y/eng.h"
#include "y/loop.h"
#include "y/rand.h"
#include "y/proc.h"
#include <glib.h>
#include <gtk/gtk.h>
#include <stdlib.h>

extern void SWIG_init (void);

// from 

static gboolean fullspeed = FALSE;
static gint seed = -1;
static GtkWidget *mainwin;

static GOptionEntry entries[] =
{
    // { "uninstalled", 0, 0, G_OPTION_ARG_NONE, &uninstalled, "assume we're running from an uninstalled source tree"},
    { "full-speed", 0, 0, G_OPTION_ARG_NONE, &fullspeed, "Run at maximum frame rate", NULL },
    { "seed", 0, 0, G_OPTION_ARG_INT, &seed, "Random number seed", "positive integer"}, 
    { NULL }
};

#if 0
static void
set_directories()
{
    scm_c_eval_string("(set! %load-should-auto-compile #f)");
    
    if (uninstalled)
    {
        use_resources = FALSE;
        directories.user_data = g_build_filename(top_builddir, "share");
        directories.user_config = g_build_filename(top_builddir, "config");
        directories.data = g_build_filename(top_builddir, "data");
        directories.scripts = g_build_filename(srcdir, "g");
        g_strdup_printf("(set! %load-path (cons \"%s\" %load-path))", directories.scripts);
        g_strdup_printf("(set! %load-compiled-path (cons \"%s\" %load-compiled-path))",
                        directories.scripts);
    }
    else
    {
        use_resources = TRUE;
        directories.user_data = g_get_user_data_dir();
        directories.user_config = g_get_user_config();
        scm_c_eval_string("(set! %load-should-auto-compile #f)");
    }
}
#endif

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
    /* Initialize memory cache */
    g_resources_register (resource_get_resource ());
    /* Create the window */
    mainwin = eng_initialize ();
    gtk_window_set_application (GTK_WINDOW (mainwin), app);
    gtk_widget_show_all (mainwin);
    /* Initialize the audio system */
    /* Load player's game options and saved game files */
    /* Create drawing surface */
    draw_initialize ();
    /* All other stuff */
    SWIG_init();

    GBytes *scheme_code = g_resources_lookup_data("/com/lonelycactus/burro/game.scm", 0, NULL);
    scm_c_eval_string(g_bytes_get_data(scheme_code, NULL));
    pm_init ();
    loop ();
}

static void
activate (GtkApplication *app)
{
    GList *list;

    list = gtk_application_get_windows (app);

    if (list)
        gtk_window_present (GTK_WINDOW (list->data));
    else
        initialize (app);
}

int
main (int argc, char **argv)
{
    GtkApplication *app;
    gint status;
    GError *error = NULL;
    GOptionContext *context;

    xscm_init_guile ();

    context = g_option_context_new ("- game options");
    g_option_context_add_main_entries (context, entries, NULL);
    g_option_context_add_group (context, gtk_get_option_group (TRUE));

    if (!g_option_context_parse (context, &argc, &argv, &error))
    {
        g_print ("option parsing failed: %s\n", error->message);
        exit (1);
    }

    xgtk_init_check (&argc, &argv);
    // game_initialize ();
    // loop_set_full_speed_flag ();

    app = gtk_application_new ("com.lonelycactus.projectburro",
                               G_APPLICATION_FLAGS_NONE);
    g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);

    status = g_application_run (G_APPLICATION (app), argc, argv);

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
