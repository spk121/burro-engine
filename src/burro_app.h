#ifndef BURRO_APP_H
#define BURRO_APP_H

#include <gtk/gtk.h>

#define BURRO_APP_TYPE (burro_app_get_type ())
G_DECLARE_FINAL_TYPE (BurroApp, burro_app, BURRO, APP, GtkApplication)

BurroApp     *burro_app_new();

#endif
