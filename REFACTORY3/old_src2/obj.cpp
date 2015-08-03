/* obj.cpp -- Object (sprite) functions

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

#include <stdbool.h>
#include <stdint.h>
#include <cairo.h>
#include <SDL.h>
#include "jsapi.hpp"
#include "Interpreter.hpp"
#include "js_func.hpp"
#include "xcairo.hpp"
#include "obj.hpp"
#include "tga.hpp"
#include "eng.hpp"
#include "const.h"

typedef struct obj_entry
{
  /** Sprite is visible if true */
  bool enable;

  /** priority aka z-level. 0 to 3 where 0 is foreground */
  int priority;

  /** location of top-left corner of sprite in sprite sheet */
  int spritesheet_i, spritesheet_j;

  /** size of sprite in pixels */
  int sprite_width, sprite_height;

  /** the "user" or screen location of the rotation center of the sprite */
  double x, y;

  /** the "device" location of the rotation center, aka, it bitmap row and column of its hotspot*/
  double rotation_center_x, rotation_center_y;

  /** the expansion factor of the sprite: 1.0 = 1 pixel per pixel */
  double expansion;

  /** the rotation angle of the sprite about its rotation center, in radians */
  double rotation;

  bool hflip;
  bool vflip;

  /** The element of the palette that corresponds to this object's first color */
  int palette_offset;
} obj_entry_t;

typedef struct obj_data
{
  uint8_t bmp[OBJSHEET_HEIGHT][OBJSHEET_WIDTH];
  uint32_t palette[OBJSHEET_PALETTE_COLORS_COUNT];
} obj_data_t;

static obj_entry_t obj[MAIN_OBJ_COUNT];
static obj_data_t objsheet[2];

/****************************************************************/

static uint32_t
adjust_colorval (uint16_t c16)
{
  uint32_t a, r, g, b, c32;
  a = (((uint32_t) c16 & 0b1000000000000000) >> 15);
  r = (((uint32_t) c16 & 0b0111110000000000) >> 10);
  g = (((uint32_t) c16 & 0b0000001111100000) >> 5);
  b = ((uint32_t) c16 & 0b0000000000011111);
  if (eng_is_colorswap ())
    {
      double temp = r;
      r = b;
      b = temp;
    } 
  if (a > 0)
    a = 0xff;
  r = r * eng_get_brightness ();
  g = g * eng_get_brightness ();
  b = b * eng_get_brightness ();
  c32 = (a << 24) + (r << 16) + (g << 8) + b;
  return c32;
}

void obj_hide (int id)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    obj[id].enable = FALSE;
}

void obj_show (int id)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    SDL_assert(obj[id].sprite_width > 0);
    SDL_assert(obj[id].sprite_height > 0);
    obj[id].enable = TRUE;
}

bool obj_is_shown (int id)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    return obj[id].enable;
}

void obj_init (int id,
               int spritesheet_i, int spritesheet_j,
               int sprite_width, int sprite_height,
               double rotation_center_x, double rotation_center_y,
               bool hflip, bool vflip,
               int palette_offset)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);

    obj[id].spritesheet_i = spritesheet_i;
    obj[id].spritesheet_j = spritesheet_j;
    obj[id].sprite_width = sprite_width;
    obj[id].sprite_height = sprite_height;
    obj[id].rotation_center_x = rotation_center_x;
    obj[id].rotation_center_y = rotation_center_y;
    obj[id].hflip = hflip;
    obj[id].vflip = vflip;
    obj[id].palette_offset = palette_offset;
}

void obj_set_spritesheet_origin (int id, int spritesheet_i, int spritesheet_j)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    SDL_assert(spritesheet_i >= 0 && spritesheet_i < OBJSHEET_WIDTH);
    SDL_assert(spritesheet_j >= 0 && spritesheet_j < OBJSHEET_HEIGHT);
    obj[id].spritesheet_i = spritesheet_i;
    obj[id].spritesheet_j = spritesheet_j;
}

void obj_set (int id, int priority,
              double x, double y,
              double rotation, double expansion,
              int palette_offset)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    SDL_assert(priority >= 0 && priority < PRIORITY_COUNT);
    SDL_assert(palette_offset >= 0 && palette_offset < OBJSHEET_PALETTE_COLORS_COUNT);
    obj[id].priority = priority;
    obj[id].x = x;
    obj[id].y = y;
    obj[id].rotation = rotation;
    obj[id].expansion = expansion;
    obj[id].palette_offset = palette_offset;
}

int obj_get_priority (int id)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    return obj[id].priority;
}

void obj_set_rotation_expansion (int id, double rotation, double expansion)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    obj[id].rotation = rotation;
    obj[id].expansion = expansion;
}

void obj_set_position (int id, double x, double y)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    obj[id].x = x;
    obj[id].y = y;
}

void obj_set_palette_offset (int id, int offset)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    obj[id].palette_offset = offset;
}

void obj_get_location (int id, double *x, double *y,
                       double *rotation_center_x, double *rotation_center_y,
                       double *rotation, double *expansion)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    *x = obj[id].x;
    *y = obj[id].y;
    *rotation_center_x = obj[id].rotation_center_x;
    *rotation_center_y = obj[id].rotation_center_y;
    *rotation = obj[id].rotation;
    *expansion = obj[id].expansion;
}

void obj_set_spritesheet_from_tga (tga_image_t *t)
{
    SDL_assert(t != NULL);

    unsigned int width, height;
    obj_data_t *osheet;
    int first = tga_get_color_map_first_index (t);
    osheet = &(objsheet[0]);
    
    tga_get_image_dimensions (t, &width, &height);
    for (unsigned int j = 0; j < height; j ++)
    {
        for (unsigned int i = 0; i < width; i ++)
        {
            osheet->bmp[j][i] = tga_get_image_data_u8_ptr(t)[j * width + i];
        }
    }
    
    for (unsigned int i = 0; i < tga_get_color_map_length (t) - first; i ++)
        osheet->palette[i] = tga_get_color_map_data_u16_ptr(t)[i + first];
}

void
obj_set_spritesheet_from_file (const char *resource)
{ 
    SDL_assert(resource != NULL);
    tga_image_t *t = tga_load_from_resource (resource);
    obj_set_spritesheet_from_tga (t);
    tga_free (t);
}


cairo_surface_t *obj_render_to_cairo_surface (int id)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    unsigned int width, height, stride;
    uint32_t *data;
    uint16_t c16;
    uint8_t index;
    cairo_surface_t *surf;
    width = obj[id].sprite_width;
    height = obj[id].sprite_height;
    
    surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
    data = xcairo_image_surface_get_argb32_data (surf);
    stride = xcairo_image_surface_get_argb32_stride (surf);
    xcairo_surface_flush (surf);
    for (unsigned int j = 0; j < height; j++)
    {
        for (unsigned int i = 0; i < width; i++)
        {
            unsigned int si, sj;
            sj = j + obj[id].spritesheet_j;
            if (obj[id].vflip == TRUE)
                sj = height - sj;
            si = i + obj[id].spritesheet_i;
            if (obj[id].hflip == TRUE)
                si = width - si;
            if (id < MAIN_OBJ_COUNT)
            {
                index = objsheet[0].bmp[sj][si];
                c16 = objsheet[0].palette[index + obj[id].palette_offset];
            }
            else
            {
                index = objsheet[1].bmp[sj][si];
                c16 = objsheet[1].palette[index + obj[id].palette_offset];
            }
            data[j * stride + i] = adjust_colorval (c16);
        }
    }
    xcairo_surface_mark_dirty (surf);
    return surf;
}

DEFINE_VOID_FUNC_INT(ObjHide, obj_hide)
DEFINE_VOID_FUNC_INT(ObjShow, obj_show)
DEFINE_BOOL_FUNC_INT(ObjIsShow, obj_is_shown)
DEFINE_VOID_FUNC_INTx5_DOUBLEx2_BOOLx2_INT(ObjInit, obj_init)
DEFINE_VOID_FUNC_INTx3(ObjSetSpritesheetOrigin, obj_set_spritesheet_origin)
DEFINE_VOID_FUNC_INTx2_DOUBLEx4_INT(ObjSet, obj_set)
DEFINE_INT_FUNC_INT(ObjGetPriority, obj_get_priority)
DEFINE_VOID_FUNC_INT_DOUBLEx2(ObjSetRotationExpansion, obj_set_rotation_expansion)
DEFINE_VOID_FUNC_INT_DOUBLEx2(ObjSetPosition, obj_set_position)
DEFINE_VOID_FUNC_INTx2(ObjSetPaletteOffset, obj_set_palette_offset)
DEFINE_VOID_FUNC_STRING(ObjSetSpritesheetFromFile, obj_set_spritesheet_from_file)

JSBool
ObjGetLocation(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    int id;

    if(!JS_ConvertArguments(cx, argc, argv, "i", &id))
       return JS_FALSE;
    double x, y, rx, ry, r, e;

    obj_get_location (id, &x, &y, &rx, &ry, &r, &e);

    JSObject *list = JS_NewArrayObject(cx, 6, NULL);
    if (list) {
        JS::Value val;
        val = DOUBLE_TO_JSVAL(x);
        JS_SetElement(cx, list, 0, &val);
        val = DOUBLE_TO_JSVAL(y);
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


JSFunctionSpec obj_functions[13] = {
DECLARE_VOID_FUNC_INT (ObjHide, obj_hide)
DECLARE_VOID_FUNC_INT (ObjShow, obj_show)
DECLARE_BOOL_FUNC_INT (ObjIsShow, obj_is_shown)
DECLARE_VOID_FUNC_INTx5_DOUBLEx2_BOOLx2_INT(ObjInit, obj_init)
DECLARE_VOID_FUNC_INTx3(ObjSetSpritesheetOrigin, obj_set_spritesheet_origin)
DECLARE_VOID_FUNC_INTx2_DOUBLEx4_INT(ObjSet, obj_set)
DECLARE_INT_FUNC_INT(ObjGetPriority, obj_get_priority)
DECLARE_VOID_FUNC_INT_DOUBLEx2(ObjSetRotationExpansion, obj_set_rotation_expansion)
DECLARE_VOID_FUNC_INT_DOUBLEx2(ObjSetPosition, obj_set_position)
DECLARE_VOID_FUNC_INTx2(ObjSetPaletteOffset, obj_set_palette_offset)
DECLARE_VOID_FUNC_STRING(ObjSetSpritesheetFromFile, obj_set_spritesheet_from_file)
JS_FS("ObjGetLocation", ObjGetLocation, 1, 0),
JS_FS_END
};
