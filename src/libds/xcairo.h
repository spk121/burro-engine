#ifndef LIBDS_XCAIRO_H
#define LIBDS_XCAIRO_H
#define COLOR_BLACK (0xFF000000)

DS_LOCAL cairo_t *
xcairo_create (ds_ctx_t *ctx, cairo_surface_t *target);
DS_LOCAL void
xcairo_destroy (ds_ctx_t *ctx, cairo_t *cr);
DS_LOCAL cairo_surface_t *
xcairo_image_surface_create (ds_ctx_t *ctx, cairo_format_t format,
			     int width, int height);
DS_LOCAL ds_error_t
xcairo_set_antialias (ds_ctx_t *ctx, cairo_t *cr, cairo_antialias_t antialias);
DS_LOCAL void
xcairo_surface_destroy (ds_ctx_t *ctx, cairo_surface_t *surface);
DS_LOCAL void
xcairo_set_source_rgb (ds_ctx_t *, cairo_t *cr, double red, double green,
		       double blue);
DS_LOCAL int
xcairo_paint (ds_ctx_t *ctx, cairo_t *cr);
DS_LOCAL int
xcairo_image_surface_get_argb32_stride (ds_ctx_t *ctx, cairo_surface_t *surface, int *stride);
DS_LOCAL int
xcairo_image_surface_get_argb32_data (ds_ctx_t *ctx, cairo_surface_t *surface,
				      uint32_t **data);

#define PTR_UNPACK24(p,c1,c2,c3)                 \
  do {                                           \
    uint8_t *__tmp = ((uint8_t *)(p));		 \
    (c3) = __tmp[0];                             \
    (c2) = __tmp[1];                             \
    (c1) = __tmp[2];                             \
  } while (0)

#endif
