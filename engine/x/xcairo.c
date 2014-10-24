#include <glib.h>
#include "xcairo.h"

static gboolean
is_valid_cairo_format_t (cairo_format_t f)
{
  return (f == CAIRO_FORMAT_ARGB32 
	  || f == CAIRO_FORMAT_RGB24
	  || f == CAIRO_FORMAT_A8
	  || f == CAIRO_FORMAT_A1
	  || f == CAIRO_FORMAT_RGB16_565);
}

static gboolean
is_valid_cairo_antialias_t (cairo_antialias_t a)
{
  return (a == CAIRO_ANTIALIAS_DEFAULT
	  || a == CAIRO_ANTIALIAS_NONE
	  || a == CAIRO_ANTIALIAS_GRAY
	  || a == CAIRO_ANTIALIAS_SUBPIXEL);
}

cairo_t *
xcairo_create (cairo_surface_t *target)
{
  cairo_t *c;
  cairo_status_t status;
  g_return_val_if_fail (target != NULL, NULL);
  g_return_val_if_fail (cairo_surface_get_reference_count (target) > 0, NULL);
  c = cairo_create (target);
  status = cairo_status (c);
  if (status != CAIRO_STATUS_SUCCESS)
    g_critical ("cairo_create was not successful");
  return c;
}

void                
xcairo_destroy (cairo_t *cr)
{
  g_return_if_fail (cr != NULL);
  cairo_destroy (cr);
}

void
xcairo_identity_matrix (cairo_t *cr)
{
  cairo_status_t status;
  g_return_if_fail (cr != NULL);
  cairo_identity_matrix (cr);
  status = cairo_status (cr);
  if (status != CAIRO_STATUS_SUCCESS)
    g_critical ("cairo_identity_matrix was not successful");
}

cairo_surface_t *
xcairo_image_surface_create (cairo_format_t format, int width, int height)
{
  cairo_surface_t *s;
  cairo_status_t status;
  g_return_val_if_fail (is_valid_cairo_format_t (format), NULL);
  g_return_val_if_fail (width > 0, NULL);
  g_return_val_if_fail (height > 0, NULL);
  s = cairo_image_surface_create (format, width, height);
  status = cairo_surface_status (s);
  if (status != CAIRO_STATUS_SUCCESS)
    g_critical ("cairo_image_surface_create was not successful");
  return s;
}

uint32_t *
xcairo_image_surface_get_argb32_data (cairo_surface_t *surface)
{
  uint32_t *data;
  g_return_val_if_fail (surface != NULL, NULL);
  g_return_val_if_fail (cairo_image_surface_get_format (surface) == CAIRO_FORMAT_ARGB32, NULL);
  g_return_val_if_fail (cairo_surface_get_reference_count (surface) > 0, NULL);
  data = (uint32_t *) cairo_image_surface_get_data (surface);
  if (data == NULL)
    g_critical ("cairo_image_surface_get_data returned NULL");
  return data;
}

int
xcairo_image_surface_get_argb32_stride (cairo_surface_t *surface)
{
  int stride;
  g_return_val_if_fail (surface != NULL, 0);
  g_return_val_if_fail (cairo_image_surface_get_format (surface) == CAIRO_FORMAT_ARGB32, 0);
  g_return_val_if_fail (cairo_surface_get_reference_count (surface) > 0, 0);
  stride = cairo_image_surface_get_stride (surface);
  if (stride <= 0)
    g_critical ("cairo_image_surface_get_stride returned invalid");
  return stride / sizeof (uint32_t);
}


void                
xcairo_paint (cairo_t *cr)
{
  cairo_status_t status;
  g_return_if_fail (cr != NULL);
  cairo_paint (cr);
  status = cairo_status (cr);
  if (status != CAIRO_STATUS_SUCCESS)
    g_critical ("cairo_paint was not successful");
}

void                
xcairo_scale (cairo_t *cr, double sx, double sy)
{
  g_return_if_fail (cr != NULL);
  g_return_if_fail (sx > 0.0);
  g_return_if_fail (sy > 0.0);
  cairo_scale (cr, sx, sy);
}

void                
xcairo_set_antialias (cairo_t *cr, cairo_antialias_t antialias)
{
  cairo_status_t status;
  g_return_if_fail (cr != NULL);
  g_return_if_fail (is_valid_cairo_antialias_t (antialias));
  cairo_set_antialias (cr, antialias);
  status = cairo_status (cr);
  if (status != CAIRO_STATUS_SUCCESS)
    g_critical ("cairo_set_antialias was not successful");
}

void
xcairo_set_matrix (cairo_t *cr, const cairo_matrix_t *matrix)
{
  cairo_status_t status;
  g_return_if_fail (cr != NULL);
  g_return_if_fail (matrix != NULL);
  cairo_set_matrix (cr, matrix);
  status = cairo_status (cr);
  if (status != CAIRO_STATUS_SUCCESS)
    g_critical ("cairo_set_matrix was not successful");
}

void
xcairo_set_source_rgb (cairo_t *cr, double red, double green, double blue)
{
  cairo_status_t status;
  g_return_if_fail (cr != NULL);
  g_return_if_fail (red >= 0.0 && red <= 1.0);
  g_return_if_fail (green >= 0.0 && green <= 1.0);
  g_return_if_fail (blue >= 0.0 && blue <= 1.0);
  cairo_set_source_rgb (cr, red, green, blue);
  status = cairo_status (cr);
  if (status != CAIRO_STATUS_SUCCESS)
    g_critical ("cairo_set_source_rgb was not successful");
}

void
xcairo_set_source_surface (cairo_t *cr, cairo_surface_t *surface, double x, double y)
{
  cairo_status_t status;
  g_return_if_fail (cr != NULL);
  g_return_if_fail (surface != NULL);
  g_return_if_fail (cairo_surface_get_reference_count (surface) > 0);
  cairo_set_source_surface (cr, surface, x, y);
  status = cairo_status (cr);
  if (status != CAIRO_STATUS_SUCCESS)
    g_critical ("cairo_set_source_surface was not successful");
}

void
xcairo_surface_destroy (cairo_surface_t *surface)
{
  g_return_if_fail (surface != NULL);
  g_return_if_fail (cairo_surface_get_reference_count (surface) > 0);
  cairo_surface_destroy (surface);
}

void
xcairo_surface_flush (cairo_surface_t *surface)
{
  cairo_status_t status;
  g_return_if_fail (surface != NULL);
  g_return_if_fail (cairo_surface_get_reference_count (surface) > 0);
  cairo_surface_flush (surface);
  status = cairo_surface_status (surface);
  if (status != CAIRO_STATUS_SUCCESS)
    g_critical ("cairo_surface_flush was not successful");
}

void                
xcairo_surface_mark_dirty (cairo_surface_t *surface)
{
  cairo_status_t status;
  g_return_if_fail (surface != NULL);
  g_return_if_fail (cairo_surface_get_reference_count (surface) > 0);
  cairo_surface_mark_dirty (surface);
  status = cairo_surface_status (surface);
  if (status != CAIRO_STATUS_SUCCESS)
    g_critical ("cairo_surface_mark_dirty was not successful");
}

