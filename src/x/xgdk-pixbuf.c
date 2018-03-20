#include "xgdk-pixbuf.h"

GdkPixbuf *
xgdk_pixbuf_new_from_file (const char *filename)
{
  GError *err = NULL;
  g_return_val_if_fail (filename != NULL, NULL);

  GdkPixbuf *pb;
  pb = gdk_pixbuf_new_from_file (filename, &err);
  if (pb == NULL)
    {
      g_critical ("gdk_pixbuf_new_from_file (%s) returned NULL: %s", filename, err->message);
      g_error_free (err);
    }
  return pb;
}

GdkPixbuf *
xgdk_pixbuf_new_from_resource (const char *resource)
{
  GError *err = NULL;
  g_return_val_if_fail (resource != NULL, NULL);

  GdkPixbuf *pb;
  pb = gdk_pixbuf_new_from_resource (resource, &err);
  if (pb == NULL)
    {
      g_critical ("gdk_pixbuf_new_from_file (%s) returned NULL: %s", resource, err->message);
      g_error_free (err);
    }
  return pb;
}

bool
xgdk_pixbuf_is_argb32 (const GdkPixbuf *pb)
{
  g_return_val_if_fail (pb != NULL, false);
  
  return ((gdk_pixbuf_get_colorspace(pb) == GDK_COLORSPACE_RGB)
	  && (gdk_pixbuf_get_bits_per_sample(pb) == 8)
	  && (gdk_pixbuf_get_has_alpha(pb))
	  && (gdk_pixbuf_get_n_channels(pb) == 4));
}

bool
xgdk_pixbuf_is_xrgb32 (const GdkPixbuf *pb)
{
  g_return_val_if_fail (pb != NULL, false);
  
  return ((gdk_pixbuf_get_colorspace(pb) == GDK_COLORSPACE_RGB)
          && (gdk_pixbuf_get_bits_per_sample(pb) == 8)
          && (!gdk_pixbuf_get_has_alpha(pb)));
}

void
xgdk_pixbuf_get_width_height_stride (const GdkPixbuf *pb,
				     int *width,
				     int *height,
				     int *stride)
{
  if (width != NULL)
    *width = 0;
  if (height != NULL)
    *height = 0;
  if (stride != NULL)
    *stride = 0;
  
  g_return_if_fail (pb != NULL);
  g_return_if_fail (xgdk_pixbuf_is_argb32 (pb) == true || xgdk_pixbuf_is_xrgb32 (pb) == true);

  if (width != NULL)
    *width = gdk_pixbuf_get_width (pb);
  if (height != NULL)
      *height = gdk_pixbuf_get_height (pb);
  if (stride != NULL)
    *stride = gdk_pixbuf_get_rowstride (pb) / 4;
  
}

uint32_t *
xgdk_pixbuf_get_argb32_pixels(GdkPixbuf *pb)
{
  g_return_val_if_fail (pb != NULL, NULL);
  g_return_val_if_fail (xgdk_pixbuf_is_argb32 (pb) == true || xgdk_pixbuf_is_xrgb32 (pb) == true, NULL);
  
  return (uint32_t *) gdk_pixbuf_get_pixels (pb);
}
