#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <cairo.h>
#include "libds.h"
#include "libds-private.h"

static bool
is_valid_cairo_antialias_t (cairo_antialias_t a)
{
  return (a == CAIRO_ANTIALIAS_DEFAULT
	  || a == CAIRO_ANTIALIAS_NONE
	  || a == CAIRO_ANTIALIAS_GRAY
	  || a == CAIRO_ANTIALIAS_SUBPIXEL);
}

static bool
is_valid_cairo_format_t (cairo_format_t f)
{
  return (f == CAIRO_FORMAT_ARGB32
	  || f == CAIRO_FORMAT_RGB24
	  || f == CAIRO_FORMAT_A8
	  || f == CAIRO_FORMAT_A1
	  || f == CAIRO_FORMAT_RGB16_565);
}

DS_LOCAL cairo_t *
xcairo_create (ds_ctx_t *ctx, cairo_surface_t *target)
{
  cairo_t *c;
  cairo_status_t status;

  if (target == NULL) {
    err (ctx, "target surface is NULL");
    return NULL;
  }
  if (cairo_surface_get_reference_count(target) <= 0) {
    err (ctx, "target surface has bad reference count %d",
	 (int) cairo_surface_get_reference_count (target));
    return NULL;
  }
  c = cairo_create (target);
  status = cairo_status (c);
  if (status != CAIRO_STATUS_SUCCESS)
    err (ctx, "cairo_create was not successful");
  return c;
}

DS_LOCAL void
xcairo_destroy (ds_ctx_t *ctx, cairo_t *cr)
{
  if (cr == NULL) {
    err (ctx, "cairo context is NULL");
    return ;
  }
  cairo_destroy (cr);
}

DS_LOCAL cairo_surface_t *
xcairo_image_surface_create (ds_ctx_t *ctx, cairo_format_t format,
			     int width, int height)
{
  cairo_surface_t *s;
  cairo_status_t status;

  if (!is_valid_cairo_format_t (format)) {
    err (ctx, "invalid cairo format: %d", (int) format);
    return NULL;
  }
  if (width <= 0) {
    err (ctx, "invalid width: %d", width);
    return NULL;
  }
  if (height <= 0) {
    err (ctx, "invalid height: %d", height);
    return NULL;
  }
  s = cairo_image_surface_create (format, width, height);
  status = cairo_surface_status (s);
  if (status != CAIRO_STATUS_SUCCESS)
    err (ctx, "cairo_image_surface_create was not successful");
  return s;
}

DS_LOCAL int
xcairo_paint (ds_ctx_t *ctx, cairo_t *cr)
{
  cairo_status_t status;
  if (cr == NULL) {
    err (ctx, "cairo context is NULL");
    return DS_ERROR_PAINT_FAILURE;
  }
  cairo_paint (cr);
  status = cairo_status (cr);
  if (status != CAIRO_STATUS_SUCCESS) {
    err (ctx, "cairo paint failure");
    return DS_ERROR_PAINT_FAILURE;
  }
  return DS_OK;
}

DS_LOCAL ds_error_t
xcairo_set_antialias (ds_ctx_t *ctx, cairo_t *cr, cairo_antialias_t antialias)
{
  cairo_status_t status;
  if (cr == NULL) {
    err (ctx, "Cairo context is NULL");
    return DS_ERROR_CONTEXT_INVALID;
  }
  if (!is_valid_cairo_antialias_t (antialias)) {
    err (ctx, "Cairo antialias type is invalid: %d", (int) antialias);
    return DS_ERROR_ANTIALIAS_OUT_OF_RANGE;
  }
  cairo_set_antialias (cr, antialias);
  status = cairo_status (cr);
  if (status != CAIRO_STATUS_SUCCESS) {
    err (ctx, "cairo_set_antialias was not successful");
    return DS_ERROR_ANTIALIAS_FAILURE;
  }
  return DS_OK;
}

DS_LOCAL void
xcairo_set_source_rgb (ds_ctx_t *ctx, cairo_t *cr, double red, double green,
		       double blue)
{
  // cairo_status_t status;
  // g_return_if_fail (cr != NULL);
  // g_return_if_fail (red >= 0.0 && red <= 1.0);
  // g_return_if_fail (green >= 0.0 && green <= 1.0);
  // g_return_if_fail (blue >= 0.0 && blue <= 1.0);
  cairo_set_source_rgb (cr, red, green, blue);
  // status = cairo_status (cr);
  // if (status != CAIRO_STATUS_SUCCESS)
  //   g_critical ("cairo_set_source_rgb was not successful");
}

DS_LOCAL void
xcairo_surface_destroy (ds_ctx_t *ctx, cairo_surface_t *surface)
{
  if (surface == NULL) {
    err (ctx, "cairo surface is NULL");
    return;
  }
  if (cairo_surface_get_reference_count (surface) > 1) {
    err (ctx, "cairo surface reference count is %d",
	 (int) cairo_surface_get_reference_count (surface));
    return;
  }
  cairo_surface_destroy (surface);
}

DS_LOCAL ds_error_t
xcairo_image_surface_get_argb32_data (ds_ctx_t *ctx, cairo_surface_t *surface,
				      uint32_t **data)
{
  *data = NULL;
  if (surface == NULL) {
    err (ctx, "cairo surface is NULL");
    return DS_ERROR_SURFACE_INVALID;
  }
  if (cairo_image_surface_get_format (surface) != CAIRO_FORMAT_ARGB32) {
    err (ctx, "cairo surface isn't ARGB32: %d",
	 cairo_image_surface_get_format (surface));
    return DS_ERROR_SURFACE_FORMAT_INCORRECT;
  }
  if (cairo_surface_get_reference_count (surface) <= 0) {
    err (ctx, "cairo surface is unreferenced: %d",
	 cairo_surface_get_reference_count (surface));
    return DS_ERROR_SURFACE_INVALID;
  }
  *data = (uint32_t *) cairo_image_surface_get_data (surface);
  if (*data == NULL) {
    err (ctx, "cairo_image_surface_get_data returned NULL");
    return DS_ERROR_SURFACE_INVALID;
  }
  return DS_OK;
}

DS_LOCAL ds_error_t
xcairo_image_surface_get_argb32_stride (ds_ctx_t *ctx,
					cairo_surface_t *surface, int *stride)
{
  if (surface == NULL) {
    err (ctx, "cairo surface is NULL");
    return DS_ERROR_SURFACE_INVALID;
  }
  if (cairo_image_surface_get_format (surface) != CAIRO_FORMAT_ARGB32) {
    err (ctx, "cairo surface isn't ARGB32: %d",
	 cairo_image_surface_get_format (surface));
    return DS_ERROR_SURFACE_FORMAT_INCORRECT;
  }
  if (cairo_surface_get_reference_count (surface) <= 0) {
    err (ctx, "cairo surface is unreferenced: %d",
	 cairo_surface_get_reference_count (surface));
    return DS_ERROR_SURFACE_INVALID;
  }
  *stride = cairo_image_surface_get_stride (surface);
  if (stride <= 0) {
    err (ctx, "cairo_image_surfac_get_stride returned invalid %d", *stride);
    return DS_ERROR_STRIDE_INVALID;
  }
  *stride /= sizeof(uint32_t);
  return DS_OK;
}
