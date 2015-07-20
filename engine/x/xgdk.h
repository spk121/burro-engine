#ifndef BURRO_XGDK_H
#define BURRO_XGDK_H

#include <gtk/gtk.h>
#include <cairo.h>
cairo_t *           xgdk_cairo_create                   (GdkWindow *window);
#endif
