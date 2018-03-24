#ifndef BURRO_APP_WIN_H
#define BURRO_APP_WIN_H

#include <gtk/gtk.h>
#include <cairo.h>
#include "burro_app.h"

#define BURRO_APP_WINDOW_TYPE (burro_app_window_get_type ())
G_DECLARE_FINAL_TYPE (BurroAppWindow, burro_app_window, BURRO, APP_WINDOW, GtkApplicationWindow)

BurroAppWindow *burro_app_window_new (BurroApp *app);
void burro_app_window_open (BurroAppWindow *win,
			    GFile *file);
gboolean canvas_draw_cb (GtkWidget *widget,
			 cairo_t *cr,
			 gpointer data);

#endif
