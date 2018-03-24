#ifndef BURRO_PREFERENCES_DIALOG_H
#define BURRO_PREFERENCES_DIALOG_H

#include <gtk/gtk.h>
#include "burro_app_win.h"

#define BURRO_TYPE_PREFERENCES_DIALOG (burro_preferences_dialog_get_type ())
G_DECLARE_FINAL_TYPE (BurroPreferencesDialog, burro_preferences_dialog, BURRO, PREFERENCES_DIALOG, GtkDialog)

BurroPreferencesDialog *burro_preferences_dialog_new (GtkWindow *win);

#endif
