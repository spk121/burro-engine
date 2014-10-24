#include "xgdk.h"

cairo_t *
xgdk_cairo_create (GdkWindow *window)
{
  cairo_t *c;
  g_return_val_if_fail (window != NULL, NULL);
  c = gdk_cairo_create (window);
  if (c == NULL)
    g_critical ("gdk_cairo_create returned NULL");
  return c;
}
