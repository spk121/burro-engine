#ifndef BURRO_XGDK_PIXBUF_H
#define BURRO_XGDK_PIXBUF_H

#include <stdint.h>
#include <stdbool.h>

#include <gdk-pixbuf/gdk-pixbuf.h>
GdkPixbuf *      xgdk_pixbuf_new_from_file       (const char *filename);
GdkPixbuf *      xgdk_pixbuf_new_from_resource   (const char *resource);
bool             xgdk_pixbuf_is_argb32           (const GdkPixbuf *pb);
bool             xgdk_pixbuf_is_xrgb32           (const GdkPixbuf *pb);
void             xgdk_pixbuf_get_width_height_stride (const GdkPixbuf *pb,
                                                      int *widght,
                                                      int *height,
                                                      int *stride);
uint32_t *       xgdk_pixbuf_get_argb32_pixels   (GdkPixbuf *pb);
#endif
