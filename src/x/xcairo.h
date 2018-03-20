/*  xcairo.h

    Copyright (C) 2018   Michael L. Gran
    This file is part of Burro Engine

    Burro Engine is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Burro Engine is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Burro Engine.  If not, see <http://www.gnu.org/licenses/>.
*/

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
/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
