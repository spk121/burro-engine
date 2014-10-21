/* xcairo.cpp -- helper functions for the Cairo library

   Copyright 2014, Michael L. Gran

   This file is part of the Project Burro game engine.

   Project Burro is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   Project Burro is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Project Burro.  If not, see
   <http://www.gnu.org/licenses/>. */

#include <cairo.h>
#include <SDL.h>
#include <stdbool.h>
#include "xcairo.hpp"

typedef cairo_surface_t xcairo_surface_t;

static bool
is_valid_cairo_format_t (cairo_format_t f)
{
  return (f == CAIRO_FORMAT_ARGB32 
	  || f == CAIRO_FORMAT_RGB24
	  || f == CAIRO_FORMAT_A8
	  || f == CAIRO_FORMAT_A1
	  || f == CAIRO_FORMAT_RGB16_565);
}

static bool
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

  SDL_assert (target != NULL);
  SDL_assert (cairo_surface_get_reference_count (target) > 0);

  c = cairo_create (target);
  status = cairo_status (c);

  SDL_assert (status == CAIRO_STATUS_SUCCESS);

  return c;
}

void                
xcairo_destroy (cairo_t *cr)
{
  SDL_assert (cr != NULL);

  cairo_destroy (cr);
}

void
xcairo_identity_matrix (cairo_t *cr)
{
  cairo_status_t status;

  SDL_assert (cr != NULL);

  cairo_identity_matrix (cr);
  status = cairo_status (cr);

  SDL_assert (status == CAIRO_STATUS_SUCCESS);
}

cairo_surface_t *
xcairo_image_surface_create (cairo_format_t format, int width, int height)
{
  cairo_surface_t *s;
  cairo_status_t status;

  SDL_assert(is_valid_cairo_format_t (format));
  SDL_assert(width > 0);
  SDL_assert(height > 0);

  s = cairo_image_surface_create (format, width, height);
  status = cairo_surface_status (s);

  SDL_assert( status == CAIRO_STATUS_SUCCESS);

  return s;
}

uint32_t *
xcairo_image_surface_get_argb32_data (cairo_surface_t *surface)
{
  uint32_t *data;

  SDL_assert (surface != NULL);
  SDL_assert (cairo_image_surface_get_format (surface) == CAIRO_FORMAT_ARGB32);
  SDL_assert (cairo_surface_get_reference_count (surface) > 0);

  data = (uint32_t *) cairo_image_surface_get_data (surface);

  SDL_assert (data != NULL);

  return data;
}

int
xcairo_image_surface_get_argb32_stride (cairo_surface_t *surface)
{
  int stride;

  SDL_assert (surface != NULL);
  SDL_assert (cairo_image_surface_get_format (surface) == CAIRO_FORMAT_ARGB32);
  SDL_assert (cairo_surface_get_reference_count (surface) > 0);

  stride = cairo_image_surface_get_stride (surface);

  SDL_assert (stride >= 0);

  return stride / sizeof (uint32_t);
}


void                
xcairo_paint (cairo_t *cr)
{
  cairo_status_t status;

  SDL_assert (cr != NULL);

  cairo_paint (cr);
  status = cairo_status (cr);

  SDL_assert (status == CAIRO_STATUS_SUCCESS);
}

void                
xcairo_scale (cairo_t *cr, double sx, double sy)
{
  SDL_assert (cr != NULL);
  SDL_assert (sx > 0.0);
  SDL_assert (sy > 0.0);

  cairo_scale (cr, sx, sy);
}

void                
xcairo_set_antialias (cairo_t *cr, cairo_antialias_t antialias)
{
  cairo_status_t status;

  SDL_assert (cr != NULL);
  SDL_assert (is_valid_cairo_antialias_t (antialias));

  cairo_set_antialias (cr, antialias);

  status = cairo_status (cr);
  SDL_assert (status == CAIRO_STATUS_SUCCESS);
}

void
xcairo_set_matrix (cairo_t *cr, const cairo_matrix_t *matrix)
{
  cairo_status_t status;

  SDL_assert (cr != NULL);
  SDL_assert (matrix != NULL);
  
  cairo_set_matrix (cr, matrix);

  status = cairo_status (cr);
  SDL_assert (status == CAIRO_STATUS_SUCCESS);
}

void
xcairo_set_source_rgb (cairo_t *cr, double red, double green, double blue)
{
  cairo_status_t status;

  SDL_assert (cr != NULL);
  SDL_assert (red >= 0.0 && red <= 1.0);
  SDL_assert (green >= 0.0 && green <= 1.0);
  SDL_assert (blue >= 0.0 && blue <= 1.0);

  cairo_set_source_rgb (cr, red, green, blue);

  status = cairo_status (cr);
  SDL_assert (status == CAIRO_STATUS_SUCCESS);
}

void
xcairo_set_source_surface (cairo_t *cr, cairo_surface_t *surface, double x, double y)
{
  cairo_status_t status;

  SDL_assert (cr != NULL);
  SDL_assert (surface != NULL);
  SDL_assert (cairo_surface_get_reference_count (surface) > 0);

  cairo_set_source_surface (cr, surface, x, y);

  status = cairo_status (cr);
  SDL_assert (status == CAIRO_STATUS_SUCCESS);
  status = cairo_surface_status (surface);
  SDL_assert (status == CAIRO_STATUS_SUCCESS);
}

void
xcairo_surface_destroy (cairo_surface_t *surface)
{
  SDL_assert (surface != NULL);
  SDL_assert (cairo_surface_get_reference_count (surface) > 0);

  cairo_surface_destroy (surface);
}

void
xcairo_surface_flush (cairo_surface_t *surface)
{
  cairo_status_t status;
  
  SDL_assert (surface != NULL);
  SDL_assert (cairo_surface_get_reference_count (surface) > 0);

  cairo_surface_flush (surface);

  status = cairo_surface_status (surface);
  SDL_assert (status == CAIRO_STATUS_SUCCESS);
}

void                
xcairo_surface_mark_dirty (cairo_surface_t *surface)
{
  cairo_status_t status;

  SDL_assert (surface != NULL);
  SDL_assert (cairo_surface_get_reference_count (surface) > 0);

  cairo_surface_mark_dirty (surface);

  status = cairo_surface_status (surface);
  SDL_assert (status == CAIRO_STATUS_SUCCESS);
}

cairo_surface_t*
xcairo_surface_reference (cairo_surface_t* surface)
{
    SDL_assert (surface != nullptr);
    cairo_surface_t *s;
    s = cairo_surface_reference (surface);
    SDL_assert (s != nullptr);

    return s;
}
