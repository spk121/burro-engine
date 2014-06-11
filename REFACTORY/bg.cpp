/* bg.cpp -- the background layers

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

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <cairo.h>
#include <SDL.h>
#include "const.h"
#include "bg.hpp"
#include "tga.hpp"
#include "xcairo.hpp"

enum bg_type_tag
{
  BG_TYPE_MAP,
  BG_TYPE_BMP8,
  BG_TYPE_BMP16,
};

typedef enum bg_type_tag bg_type_t;

enum bg_index_tag
{
  BG_MAIN_0 = 0,
  BG_MAIN_1 = 1,
  BG_MAIN_2 = 2,
  BG_MAIN_3 = 3,
};

typedef enum bg_index_tag bg_index_t;

struct bg_map_data
{
  int height;
  int width;
  uint16_t map[MAP_HEIGHT][MAP_WIDTH];
  uint8_t tilesheet[TILESHEET_HEIGHT][TILESHEET_WIDTH];
  uint16_t palette[PALETTE_COLORS_COUNT];
};

struct bg_bmp8_data
{
  int height;
  int width;
  uint8_t bmp[BMP8_HEIGHT][BMP8_WIDTH];
  uint16_t palette[PALETTE_COLORS_COUNT];
};

struct bg_bmp16_data
{
  int height;
  int width;
  uint16_t bmp[BMP16_HEIGHT][BMP16_WIDTH];
};


typedef struct bg_entry
{
  /* BG is display when true */
  bool enable;

  /** tile and map, palette bmp, or true color bmp */
  bg_type_t type;

  /* z-level: 0 is foreground, 3 is background */
  int priority;

  /** the "user" or screen location of the rotation center of the background */
  double scroll_x, scroll_y;

  /** the "device" location of the rotation center of the background*/
  double rotation_center_x, rotation_center_y;

  /** the expansion factor of the background: 1.0 = 1 pixel per pixel */
  double expansion;

  /** the rotation angle of the background about its rotation center, in radians */
  double rotation;

  union
  {
    struct bg_map_data map;
    struct bg_bmp8_data bmp8;
    struct bg_bmp16_data bmp16;
  };
} bg_entry_t;

struct bg_tag
{
  bool colorswap;
  double brightness;

  bg_entry_t bg[MAIN_BACKGROUNDS_COUNT];
};

typedef struct bg_tag bg_t;

static bg_t bg;

static cairo_surface_t *bg_render_map_to_cairo_surface (int id);
static cairo_surface_t *bg_render_bmp8_to_cairo_surface (int id);
static cairo_surface_t *bg_render_bmp16_to_cairo_surface (int id);

static uint32_t
adjust_colorval (uint16_t c16)
{
  uint32_t a, r, g, b, c32;

  a = (((uint32_t) c16 & 0b1000000000000000) >> 15);
  r = (((uint32_t) c16 & 0b0111110000000000) >> 10);
  g = (((uint32_t) c16 & 0b0000001111100000) >> 5);
  b = ((uint32_t) c16 & 0b0000000000011111);

  if (a == 1 || (r == 0x1f && g == 0x0 && b == 0x1f))
    {
      /* Transparent */
      return 0x0;
    }

  if (bg.colorswap)
    {
      double temp = r;
      r = b;
      b = temp;
    }
  a = 0xff;
  r = r * bg.brightness;
  g = g * bg.brightness;
  b = b * bg.brightness;
  /* Add 3 extra bits to promote the 5-bit val to 8-bit val */
  c32 = (a << 24) + (r << 19) + (g << 11) + (b << 3);
  return c32;
}


void
bg_set_brightness (double brightness)
{
  SDL_assert (brightness >= 0.0);
  SDL_assert (brightness <= 1.0);

  bg.brightness = brightness;
}

double
bg_get_brightness (void)
{
  return bg.brightness;
}

void
bg_set_colorswap (bool flag)
{
  SDL_assert (flag == 0 || flag == 1);
  bg.colorswap = flag;
}

bool
bg_get_colorswap (void)
{
  return bg.colorswap;
}

bool
bg_is_shown (int id)
{
  SDL_assert (id >= 0 && id < MAIN_BACKGROUNDS_COUNT);
  return bg.bg[id].enable;
}

static uint16_t *
bg_get_map_ptr (int id)
{
  SDL_assert (id >= 0 && id < MAIN_BACKGROUNDS_COUNT);
  return &(bg.bg[id].map.map[0][0]);
}

static uint8_t *
bg_get_tilesheet_ptr (int id)
{
  return &(bg.bg[id].map.tilesheet[0][0]);
}

static uint8_t *
bg_get_bmp8_ptr (int id)
{
  return &(bg.bg[id].bmp8.bmp[0][0]);
}

static uint16_t *
bg_get_bmp16_ptr (int id)
{
  return &(bg.bg[id].bmp16.bmp[0][0]);
}

int
bg_get_priority (int id)
{
  return bg.bg[id].priority;
}

void
bg_hide (int id)
{
  bg.bg[id].enable = FALSE;
}

void
bg_init (int id, bg_type_t type, int width, int height)
{
    bg.bg[id].type = type;
    bg.bg[id].scroll_x = 0.0;
    bg.bg[id].scroll_y = 0.0;
    bg.bg[id].rotation_center_x = 0.0;
    bg.bg[id].rotation_center_y = 0.0;
    bg.bg[id].expansion = 1.0;
    bg.bg[id].rotation = 0.0;
    switch (type)
    {
    case BG_TYPE_MAP:
        bg.bg[id].map.height = height;
        bg.bg[id].map.width = width;
        break;
    case BG_TYPE_BMP8:
        bg.bg[id].bmp8.height = height;
        bg.bg[id].bmp8.width = width;
        break;
    case BG_TYPE_BMP16:
        bg.bg[id].bmp16.height = height;
        bg.bg[id].bmp16.width = width;
        break;
    }
}

void
bg_rotate (int id, double angle)
{
    bg.bg[id].rotation += angle;
}

void
bg_scroll (int id, double dx, double dy)
{
    bg.bg[id].scroll_x += dx;
    bg.bg[id].scroll_y += dy;
}

void
bg_set (int id, double rotation, double expansion, double scroll_x,
        double scroll_y, double rotation_center_x, double rotation_center_y)
{
    bg.bg[id].rotation = rotation;
    bg.bg[id].expansion = expansion;
    bg.bg[id].scroll_x = scroll_x;
    bg.bg[id].scroll_y = scroll_y;
    bg.bg[id].rotation_center_x = rotation_center_x;
    bg.bg[id].rotation_center_y = rotation_center_y;
}

void
bg_set_rotation_center (int id, double rotation_center_x,
			double rotation_center_y)
{
  bg.bg[id].rotation_center_x = rotation_center_x;
  bg.bg[id].rotation_center_y = rotation_center_y;
}

void
bg_set_priority (int id, int priority)
{
  bg.bg[id].priority = priority;
}

void
bg_set_rotation (int id, double rotation)
{
  bg.bg[id].rotation = rotation;
}

void
bg_set_rotation_expansion (int id, double rotation, double expansion)
{
  bg.bg[id].rotation = rotation;
  bg.bg[id].expansion = expansion;
}

void
bg_set_expansion (int id, double expansion)
{
  bg.bg[id].expansion = expansion;
}

void
bg_show (int id)
{
  bg.bg[id].enable = TRUE;
}

static void
bg_set_map_from_tga (int id, tga_image_t * t)
{
    unsigned int width, height;
    tga_get_image_dimensions (t, &width, &height);
    bg.bg[id].map.width = width;
    bg.bg[id].map.height = height;
    bg.bg[id].type = BG_TYPE_MAP;
    for (unsigned int j = 0; j < height; j++)
    {
        for (unsigned int i = 0; i < width; i++)
        {
            bg.bg[id].map.map[j][i] =
                tga_get_image_data_u16_ptr (t)[j * width + i];
        }
    }
}

static void
bg_set_tilesheet_from_tga (int id, tga_image_t * t)
{
    unsigned int width, height;
    unsigned int first = tga_get_color_map_first_index (t);
    
    tga_get_image_dimensions (t, &width, &height);
    bg.bg[id].type = BG_TYPE_MAP;
    
    for (unsigned int j = 0; j < height; j++)
    {
        for (unsigned int i = 0; i < width; i++)
        {
            bg.bg[id].map.tilesheet[j][i] =
                tga_get_image_data_u8_ptr (t)[j * width + i];
        }
    }
    
    SDL_assert(tga_get_color_map_length (t) > first);
    for (unsigned int i = 0; i < tga_get_color_map_length (t) - first; i++)
        bg.bg[id].map.palette[i] = tga_get_color_map_data_u16_ptr (t)[i + first];
}

static void
bg_set_bmp8_from_tga (int id, tga_image_t * t)
{
    unsigned int width, height;
    unsigned int first = tga_get_color_map_first_index (t);
    
    tga_get_image_dimensions (t, &width, &height);
    bg.bg[id].bmp8.width = width;
    bg.bg[id].bmp8.height = height;
    bg.bg[id].type = BG_TYPE_BMP8;
    for (unsigned int j = 0; j < height; j++)
    {
        for (unsigned int i = 0; i < width; i++)
        {
            bg.bg[id].bmp8.bmp[j][i] =
                tga_get_image_data_u8_ptr (t)[j * width + i];
        }
    }
    
    SDL_assert(tga_get_color_map_length (t) > first);
    for (unsigned int i = 0; i < tga_get_color_map_length (t) - first; i++)
        bg.bg[id].bmp8.palette[i] = tga_get_color_map_data_u16_ptr (t)[i + first];
}

void
bg_set_bmp8_from_resource (int id, const char *resource)
{
  tga_image_t *t = tga_load_from_resource (resource);
  bg_set_bmp8_from_tga (id, t);
  tga_free (t);
}

static void
bg_set_bmp16_from_tga (int id, tga_image_t * t)
{
    unsigned int width, height;
    tga_hflip_t hflip;
    tga_vflip_t vflip;
    tga_get_image_dimensions (t, &width, &height);
    tga_get_image_orientation (t, &hflip, &vflip);
    bg.bg[id].bmp16.width = width;
    bg.bg[id].bmp16.height = height;
    bg.bg[id].type = BG_TYPE_BMP16;
    for (unsigned int j = 0; j < height; j++)
    {
        for (unsigned int i = 0; i < width; i++)
        {
            unsigned int i2, j2;

            SDL_assert (width >= i + 1);
            SDL_assert (height >= j + 1);

            if (hflip == RIGHT_TO_LEFT)
                i2 = width - 1 - i;
            else
                i2 = i;
            if (vflip == BOTTOM_TO_TOP)
                j2 = height - 1 - j;
            else
                j2 = j;
            bg.bg[id].bmp16.bmp[j2][i2] =
                tga_get_image_data_u16_ptr (t)[j * width + i];
        }
    }
}

void
bg_set_bmp16_from_resource (int id, const char *resource)
{
  tga_image_t *t = tga_load_from_resource (resource);
  bg_set_bmp16_from_tga (id, t);
  tga_free (t);
}

cairo_surface_t *
bg_render_to_cairo_surface (int id)
{
  switch (bg.bg[id].type)
    {
    case BG_TYPE_MAP:
      return bg_render_map_to_cairo_surface (id);
      break;
    case BG_TYPE_BMP8:
      return bg_render_bmp8_to_cairo_surface (id);
      break;
    case BG_TYPE_BMP16:
      return bg_render_bmp16_to_cairo_surface (id);
      break;
    default:
      abort ();
      break;
    }
}

static cairo_surface_t *
bg_render_map_to_cairo_surface (int id)
{
  cairo_surface_t *surf;
  uint32_t *data;
  int stride;
  int tile_j, tile_i, delta_tile_i, delta_tile_j;
  int map_index;
  uint32_t index, c;
  int width, height;

  width = bg.bg[id].map.width;
  height = bg.bg[id].map.height;
  surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32,
				      width * TILE_WIDTH,
				      height * TILE_HEIGHT);
  data = xcairo_image_surface_get_argb32_data (surf);
  stride = xcairo_image_surface_get_argb32_stride (surf);
  xcairo_surface_flush (surf);

  for (int map_j = 0; map_j < height; map_j++)
    {
      for (int map_i = 0; map_i < width; map_i++)
	{
	  /* Fill in the tile brush */
	  map_index = bg.bg[id].map.map[map_j][map_i];
	  delta_tile_j = (map_index / TILESHEET_WIDTH_IN_TILES) * TILE_HEIGHT;
	  delta_tile_i = (map_index % TILESHEET_WIDTH_IN_TILES) * TILE_WIDTH;
	  for (tile_j = 0; tile_j < TILE_HEIGHT; tile_j++)
	    {
	      for (tile_i = 0; tile_i < TILE_WIDTH; tile_i++)
		{
		  index =
		    bg.bg[id].map.tilesheet[delta_tile_j +
					    tile_j][delta_tile_i + tile_i];

		  c = adjust_colorval (bg.bg[id].map.palette[index]);
		  data[(map_j * TILE_HEIGHT + tile_j) * stride +
		       (map_i * TILE_WIDTH + tile_i)] = c;
		}
	    }
	}
    }
  xcairo_surface_mark_dirty (surf);
  return surf;
}

static cairo_surface_t *
bg_render_bmp8_to_cairo_surface (int id)
{
  int width, height, stride;
  uint32_t *data;
  uint16_t c16;
  uint8_t index;
  cairo_surface_t *surf;
  width = bg.bg[id].bmp8.width;
  height = bg.bg[id].bmp8.height;

  surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
  data = xcairo_image_surface_get_argb32_data (surf);
  stride = xcairo_image_surface_get_argb32_stride (surf);
  xcairo_surface_flush (surf);
  for (int j = 0; j < height; j++)
    {
      for (int i = 0; i < width; i++)
	{
	  index = bg.bg[id].bmp8.bmp[j][i];
	  c16 = bg.bg[id].bmp8.palette[index];
	  data[j * stride + i] = adjust_colorval (c16);
	}
    }
  xcairo_surface_mark_dirty (surf);
  return surf;
}

static cairo_surface_t *
bg_render_bmp16_to_cairo_surface (int id)
{
  int width, height, stride;
  uint32_t *data;
  uint16_t c16;
  cairo_surface_t *surf;
  width = bg.bg[id].bmp16.width;
  height = bg.bg[id].bmp16.height;

  surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
  data = xcairo_image_surface_get_argb32_data (surf);
  stride = xcairo_image_surface_get_argb32_stride (surf);
  xcairo_surface_flush (surf);
  for (int j = 0; j < height; j++)
    {
      for (int i = 0; i < width; i++)
	{
	  c16 = bg.bg[id].bmp16.bmp[j][i];
	  data[j * stride + i] = adjust_colorval (c16);
	}
    }
  xcairo_surface_mark_dirty (surf);
  return surf;
}

void
bg_get_transform (int id, double *scroll_x, double *scroll_y,
		  double *rotation_center_x, double *rotation_center_y,
		  double *rotation, double *expansion)
{
  *scroll_x = bg.bg[id].scroll_x;
  *scroll_y = bg.bg[id].scroll_y;
  *rotation_center_x = bg.bg[id].rotation_center_x;
  *rotation_center_y = bg.bg[id].rotation_center_y;
  *rotation = bg.bg[id].rotation;
  *expansion = bg.bg[id].expansion;
}

static void compute_transform (cairo_matrix_t *matrix,
                               double rotation_center_screen_x,
                               double rotation_center_screen_y,
                               int rotation_center_bitmap_row,
                               int rotation_center_bitmap_column,
                               double rotation_angle,
                               double expansion_factor)
{
    double xx, xy, yx, yy, x0, y0;
    double sn, cs;
    if (expansion_factor == 0.0)
        expansion_factor = 1.0;
    sn = sin (rotation_angle);
    cs = cos (rotation_angle);
    xx = expansion_factor * cs;
    xy = expansion_factor * sn;
    yx = -xy;
    yy = xx;
    x0 = (rotation_center_screen_x
          - (xx * (double)rotation_center_bitmap_column
             + xy * (double)rotation_center_bitmap_row));
    y0 = (rotation_center_screen_y
          - (yx * (double)rotation_center_bitmap_column
             + yy * (double)rotation_center_bitmap_row));
    matrix->xx = xx;
    matrix->xy = xy;
    matrix->yx = yx;
    matrix->yy = yy;
    matrix->x0 = x0;
    matrix->y0 = y0;
}

static void paint_transformed_image (cairo_t *context,
                                     cairo_matrix_t *matrix,
                                     cairo_surface_t *surface)
{
    /* Set the coordinate transform */
    xcairo_set_matrix (context, matrix);
    
    /* Now copy it to the screen */
    xcairo_set_source_surface (context, surface, 0, 0);
    xcairo_paint (context);
    
    /* Restore the coordinate system to normal */
    xcairo_identity_matrix (context);
}

static void draw_background_layer (cairo_t *screen, int layer)
{
    cairo_surface_t *surf;
    cairo_matrix_t matrix;
    double scroll_x, scroll_y, rotation_center_x, rotation_center_y;
    double rotation, expansion;
    
    surf = bg_render_to_cairo_surface (layer);
    xcairo_surface_mark_dirty (surf);
    bg_get_transform (layer, &scroll_x, &scroll_y,
                      &rotation_center_x, &rotation_center_y,
                      &rotation, &expansion);
    compute_transform (&matrix, scroll_x, scroll_y,
                       rotation_center_x, rotation_center_y,
                       rotation, expansion);
    paint_transformed_image (screen, &matrix, surf);
    xcairo_surface_destroy (surf);
}


void
bg_draw_zlevel(cairo_t *screen, int priority)
{
    for (int layer = MAIN_BACKGROUNDS_COUNT - 1; layer >= 0; layer --)
    {
        if (bg_is_shown (layer) && bg_get_priority (layer) == priority)
            draw_background_layer (screen, layer);
    }
}


JSBool
BgSetBrightnessFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    double brightness;

    if(!JS_ConvertArguments(cx, argc, argv, "d", &brightness))
       return JS_FALSE;
    
    bg_set_brightness(brightness);

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgGetBrightnessFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    JS_SET_RVAL(cx, vp, JS_NumberValue(bg_get_brightness()));
    return JS_TRUE;
}

JSBool
BgSetColorswapFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    JSBool colorswap;

    if(!JS_ConvertArguments(cx, argc, argv, "b", &colorswap))
       return JS_FALSE;
    
    bg_set_colorswap((bool) colorswap);

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgGetColorswapFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    JS::Value ret;
    ret.setBoolean(bg_get_colorswap());
    JS_SET_RVAL(cx, vp, ret);
    return JS_TRUE;
}

JSBool
BgSetBmp8FromFileFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;
    JSString *filename = NULL;

    if(!JS_ConvertArguments(cx, argc, argv, "iS", &id, &filename))
       return JS_FALSE;

    if (filename) {
        char *str;
        str = JS_EncodeString(cx, filename);
        bg_set_bmp8_from_resource(id, str);
        free(str);
    }

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgSetBmp16FromFileFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;
    JSString *filename = NULL;

    if(!JS_ConvertArguments(cx, argc, argv, "iS", &id, &filename))
       return JS_FALSE;

    if (filename) {
        char *str;
        str = JS_EncodeString(cx, filename);
        bg_set_bmp16_from_resource(id, str);
        free(str);
    }

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool BgIsShownFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;

    if(!JS_ConvertArguments(cx, argc, argv, "i", &id))
       return JS_FALSE;
    
    JS::Value ret;
    ret.setBoolean(bg_is_shown(id));
    JS_SET_RVAL(cx, vp, ret);
    return JS_TRUE;
}

JSBool BgGetPriorityFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;

    if(!JS_ConvertArguments(cx, argc, argv, "i", &id))
       return JS_FALSE;
    
    JS::Value ret;
    ret.setNumber((uint32_t) bg_get_priority(id));
    JS_SET_RVAL(cx, vp, ret);
    return JS_TRUE;
}

JSBool
BgHideFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;

    if(!JS_ConvertArguments(cx, argc, argv, "i", &id))
       return JS_FALSE;
    
    bg_hide(id);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgInitFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id, type, width, height;

    if(!JS_ConvertArguments(cx, argc, argv, "iiii", &id, &type, &width, &height))
       return JS_FALSE;

    bg_init(id, (bg_type_t) type, width, height);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgSetPriorityFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id, priority;

    if(!JS_ConvertArguments(cx, argc, argv, "ii", &id, &priority))
       return JS_FALSE;

    bg_set_priority(id, priority);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgShowFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;

    if(!JS_ConvertArguments(cx, argc, argv, "i", &id))
       return JS_FALSE;
    
    bg_show(id);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgRotateFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;
    double rotation;

    if(!JS_ConvertArguments(cx, argc, argv, "id", &id, &rotation))
       return JS_FALSE;

    bg_rotate(id, rotation);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgScrollFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;
    double dx, dy;

    if(!JS_ConvertArguments(cx, argc, argv, "idd", &id, &dx, &dy))
       return JS_FALSE;

    bg_scroll(id, dx, dy);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgSetFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;
    double rotation, expansion, scroll_x, scroll_y,
        center_x, center_y;

    if(!JS_ConvertArguments(cx, argc, argv, "idddddd", &id, 
                            &rotation, &expansion, &scroll_x, &scroll_y,
                            &center_x, &center_y))
        return JS_FALSE;

    bg_set(id, rotation, expansion, scroll_x, scroll_y,
           center_x, center_y);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgSetRotationCenterFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;
    double rx, ry;

    if(!JS_ConvertArguments(cx, argc, argv, "idd", &id, &rx, &ry))
       return JS_FALSE;

    bg_set_rotation_center(id, rx, ry);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgSetRotationFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;
    double rotation;

    if(!JS_ConvertArguments(cx, argc, argv, "id", &id, &rotation))
       return JS_FALSE;

    bg_set_rotation(id, rotation);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgSetRotationExpansionFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;
    double rotation, expansion;

    if(!JS_ConvertArguments(cx, argc, argv, "idd", &id, &rotation, &expansion))
       return JS_FALSE;

    bg_set_rotation_expansion(id, rotation, expansion);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgSetExpansionFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;
    double expansion;

    if(!JS_ConvertArguments(cx, argc, argv, "id", &id, &expansion))
       return JS_FALSE;

    bg_set_expansion(id, expansion);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
    return JS_TRUE;
}

JSBool
BgGetTransformFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;

    if(!JS_ConvertArguments(cx, argc, argv, "i", &id))
       return JS_FALSE;

    double sx, sy, rx, ry, r, e;
    bg_get_transform(id, &sx, &sy, &rx, &ry, &r, &e);

    JSObject *list = JS_NewArrayObject(cx, 6, NULL);
    if (list) {
        JS::Value val;
        val = DOUBLE_TO_JSVAL(sx);
        JS_SetElement(cx, list, 0, &val);
        val = DOUBLE_TO_JSVAL(sy);
        JS_SetElement(cx, list, 1, &val);
        val = DOUBLE_TO_JSVAL(rx);
        JS_SetElement(cx, list, 2, &val);
        val = DOUBLE_TO_JSVAL(ry);
        JS_SetElement(cx, list, 3, &val);
        val = DOUBLE_TO_JSVAL(r);
        JS_SetElement(cx, list, 4, &val);
        val = DOUBLE_TO_JSVAL(e);
        JS_SetElement(cx, list, 5, &val);
        JS_SET_RVAL(cx, vp, OBJECT_TO_JSVAL(list));
        return JS_TRUE;
    }
    return JS_FALSE;
}

JSFunctionSpec bg_functions[20] = {
    JS_FS("BgSetBrightness",  BgSetBrightnessFunc, 1, 0),
    JS_FS("BgGetBrightness",  BgGetBrightnessFunc, 0, 0),
    JS_FS("BgSetColorswap",   BgSetColorswapFunc, 1, 0),
    JS_FS("BgGetColorswap",   BgGetColorswapFunc, 0, 0),
    JS_FS("BgSetBmp8FromFile", BgSetBmp8FromFileFunc, 2, 0),
    JS_FS("BgSetBmp16FromFile", BgSetBmp16FromFileFunc, 2, 0),
    JS_FS("BgIsShown",        BgIsShownFunc, 1, 0),
    JS_FS("BgGetPriority",    BgGetPriorityFunc, 1, 0),
    JS_FS("BgHide",           BgHideFunc, 1, 0),
    JS_FS("BgInit",           BgInitFunc, 4, 0),
    JS_FS("BgSetPriority",    BgSetPriorityFunc, 2, 0),
    JS_FS("BgShow",           BgShowFunc, 1, 0),
    JS_FS("BgRotate",         BgRotateFunc, 2, 0),
    JS_FS("BgScroll",         BgScrollFunc, 3, 0),
    JS_FS("BgSet",            BgSetFunc, 7, 0),
    JS_FS("BgSetRotationCenter", BgSetRotationCenterFunc, 3, 0),
    JS_FS("BgSetRotation",    BgSetRotationFunc, 2, 0),
    JS_FS("BgSetRotationExpansion", BgSetRotationExpansionFunc, 3, 0),
    JS_FS("BgGetTransform",   BgGetTransformFunc, 1, 0),
    JS_FS_END
};
