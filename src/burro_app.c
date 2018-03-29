#include <gtk/gtk.h>

#include "burro_app.h"
#include "burro_app_win.h"
#include "burro_preferences_dialog.h"

struct _BurroApp
{
    GtkApplication parent;
};

G_DEFINE_TYPE(BurroApp, burro_app, GTK_TYPE_APPLICATION);

static void
action_quit (GSimpleAction *action,
             GVariant *parameter,
             gpointer app)
{
    g_application_quit (G_APPLICATION (app));
}

static GActionEntry app_entries[] =
{
    {"quit", action_quit, NULL, NULL, NULL},
};


static void
burro_app_startup (GApplication *application)
{
    BurroApp *app = BURRO_APP (application);
    GtkBuilder *builder;
    GMenuModel *app_menu;

    G_APPLICATION_CLASS (burro_app_parent_class)->startup (application);

    g_action_map_add_action_entries (G_ACTION_MAP (app),
                                     app_entries, G_N_ELEMENTS (app_entries),
                                     app);
    
    builder = gtk_builder_new_from_resource ("/com/lonelycactus/burroengine/app-menu.ui");

    app_menu = G_MENU_MODEL (gtk_builder_get_object (builder, "appmenu"));
    gtk_application_set_app_menu (GTK_APPLICATION (app), app_menu);
    g_object_unref (builder);

    /* Set GNOME properties that PulseAudio likes to have */
    g_set_application_name("Burro Engine");
    g_setenv("PULSE_PROP_media.role", "game", TRUE);
    gtk_window_set_default_icon_name("applications-games");
}

static void
burro_app_activate (GApplication *app)
{
    BurroAppWindow *win;

    // FIXME: when we open via ACTIVATE, show the GEARS menu?
    
    win = burro_app_window_new (BURRO_APP (app));
    burro_app_window_open (win, NULL);
    gtk_window_present (GTK_WINDOW (win));
}

static void
burro_app_open (GApplication *app,
                GFile **files,
                gint n_files,
                const gchar *hint)
{
    BurroAppWindow *win;

    // FIXME: when we open via OPEN, hide the GEARS menu?
    
    if (n_files != 1)
        g_warning ("Asked to open %d files, but, can only open 1", n_files);
    
    // We can only handle one file.
    win = burro_app_window_new (BURRO_APP (app));
    burro_app_window_open (win, files[0]);
    gtk_window_present (GTK_WINDOW (win));
}

static void
burro_app_class_init (BurroAppClass *class)
{
    G_APPLICATION_CLASS (class)->startup = burro_app_startup;
    G_APPLICATION_CLASS (class)->activate = burro_app_activate;
    G_APPLICATION_CLASS (class)->open = burro_app_open;
}

static void
burro_app_init (BurroApp *app)
{
}


BurroApp *
burro_app_new (void)
{
    return g_object_new (BURRO_APP_TYPE,
                         "application-id", "com.lonelycactus.burroengine",
                         "flags", (G_APPLICATION_NON_UNIQUE
                                   | G_APPLICATION_HANDLES_OPEN),
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
