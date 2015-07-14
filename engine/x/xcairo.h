#ifndef BURRO_XCAIRO_H
#define BURRO_XCAIRO_H
#include <stdint.h>
#include <cairo.h>

#ifdef  __cplusplus
extern "C" {
#endif

cairo_t *           xcairo_create                       (cairo_surface_t *target);
void                xcairo_destroy                      (cairo_t *cr);
void                xcairo_identity_matrix              (cairo_t *cr);
cairo_surface_t *   xcairo_image_surface_create         (cairo_format_t format,
                                                         int width,
                                                         int height);
uint32_t *          xcairo_image_surface_get_argb32_data (cairo_surface_t *surface);
int                 xcairo_image_surface_get_argb32_stride (cairo_surface_t *surface);
void                xcairo_paint                        (cairo_t *cr);
void                xcairo_scale                        (cairo_t *cr,
                                                         double sx,
                                                         double sy);
void                xcairo_set_antialias                (cairo_t *cr,
                                                         cairo_antialias_t antialias);
void                xcairo_set_matrix                   (cairo_t *cr,
                                                         const cairo_matrix_t *matrix);
void                xcairo_set_source_rgb               (cairo_t *cr,
                                                         double red,
                                                         double green,
                                                         double blue);
void                xcairo_set_source_surface           (cairo_t *cr,
                                                         cairo_surface_t *surface,
                                                         double x,
                                                         double y);
void                xcairo_surface_destroy              (cairo_surface_t *surface);
void                xcairo_surface_flush                (cairo_surface_t *surface);
void                xcairo_surface_mark_dirty           (cairo_surface_t *surface);

#ifdef  __cplusplus
}
#endif

  
#endif
