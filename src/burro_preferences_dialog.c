#include <gtk/gtk.h>

#include "burro_app.h"
#include "burro_app_win.h"
#include "burro_preferences_dialog.h"
#include "burro_config_keys.h"

struct _BurroPreferencesDialog
{
    GtkDialog      parent;
    GSettings      *settings;
    GtkCheckButton *repl_check;
    GtkCheckButton *message_check;
};

G_DEFINE_TYPE(BurroPreferencesDialog, burro_preferences_dialog, GTK_TYPE_DIALOG);

static void
burro_preferences_dialog_init (BurroPreferencesDialog *prefs)
{
    gtk_widget_init_template (GTK_WIDGET (prefs));

    prefs->settings = g_settings_new (BURRO_CONF_DOMAIN);

    g_settings_bind (prefs->settings, BURRO_CONF_REPL,
                     prefs->repl_check, "active",
                     G_SETTINGS_BIND_DEFAULT);
    g_settings_bind (prefs->settings, BURRO_CONF_DEBUG,
                     prefs->message_check, "active",
                     G_SETTINGS_BIND_DEFAULT);
}

static void
burro_preferences_dialog_dispose (GObject *object)
{
    BurroPreferencesDialog *prefs = BURRO_PREFERENCES_DIALOG (object);
    g_clear_object (&prefs->settings);

    G_OBJECT_CLASS (burro_preferences_dialog_parent_class)->dispose (object);
}

static void
burro_preferences_dialog_class_init (BurroPreferencesDialogClass *class)
{
    GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (class);

    gtk_widget_class_set_template_from_resource (widget_class,
                                                 "/com/lonelycactus/burroengine/burro_preferences_dialog.ui");
    gtk_widget_class_bind_template_child (widget_class,
                                          BurroPreferencesDialog,
                                          repl_check);
    gtk_widget_class_bind_template_child (widget_class,
                                          BurroPreferencesDialog,
                                          message_check);
    
    G_OBJECT_CLASS (class)->dispose = burro_preferences_dialog_dispose;
}

BurroPreferencesDialog *
burro_preferences_dialog_new (GtkWindow *win)
{
    return g_object_new (BURRO_TYPE_PREFERENCES_DIALOG,
                         "transient-for", win,
                         "use-header-bar", TRUE,
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

