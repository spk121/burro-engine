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
//#include "obj.hpp"
#include "tga.hpp"
#include "eng.hpp"
#include "const.h"
#include "Object.hpp"
#include "ResCache.hpp"

Object_entry obj[MAIN_OBJ_COUNT];
cairo_surface_t *objsheet;

static unsigned int quick_hash(const string& str)
{
    unsigned int val = 0;
    for (size_t i = 0; i < str.size(); i ++) {
        if (str[i] == '\0')
            break;
        val = val * 101 + (unsigned char) str[i];
        val = val & 0xFFFF;
    }
    // SDL_LogDebug(SDL_LOG_CATEGORY_APPLICATION, "Tmx Quick Hash: \"%s\" = %x", str.c_str(), val);
    return val;
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

void obj_hide (int id)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    obj[id].enable = FALSE;
}

void obj_init (int id,
               int spritesheet_i, int spritesheet_j,
               int sprite_width, int sprite_height,
               double rotation_center_x, double rotation_center_y,
               bool hflip, bool vflip, int dummy)
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
              double rotation, double expansion, int dummy)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    SDL_assert(priority >= 0 && priority < PRIORITY_COUNT);
    obj[id].priority = priority;
    obj[id].x = x;
    obj[id].y = y;
    obj[id].rotation = rotation;
    obj[id].expansion = expansion;
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

void obj_update_spritesheet(const string& name)
{
    // FIXME: check name is a png
    cairo_surface_t* tilesheet_surface = (cairo_surface_t*) resource_cache.Get(quick_hash(name));
    if (tilesheet_surface)
    {
        // FIXME: free old tilesheet
        objsheet = tilesheet_surface;
        // FIXME: assert the objsheet is argb32 here
    }
}

static JSBool
objGetJsonString (JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    JSString *js_string = NULL;
    if (!JS_ConvertArguments(cx, argc, argv, "S", &js_string))
        return JS_FALSE;
    if (js_string) {
        string name ={JS_EncodeString(cx, js_string)};
        char *json = (char *) resource_cache.Get(quick_hash(name));
        JSString *js_json = JS_NewStringCopyN(cx, json, strlen(json));
        JS::Value ret;
        ret.setString(js_json);
        JS_SET_RVAL(cx, vp, ret);
        return JS_TRUE;
    }
    return JS_FALSE;
}

static uint32_t
adjust_colorval (uint32_t c32)
{
    uint32_t a, r, g, b;
    a = (((uint32_t) c32 & 0xFF000000) >> 24);
    r = (((uint32_t) c32 & 0x00FF0000) >> 16);
    g = (((uint32_t) c32 & 0x0000FF00) >> 8);
    b = ((uint32_t) c32 & 0x000000FF);
    if (eng_is_colorswap ())
    {
        auto temp = r;
        r = b;
        b = temp;
    }
    r = r * eng_get_brightness ();
    g = g * eng_get_brightness ();
    b = b * eng_get_brightness ();
    return  (a << 24) + (r << 16) + (g << 8) + b;
}


cairo_surface_t *obj_render_to_cairo_surface (int id)
{
    SDL_assert(id >= 0 && id < MAIN_OBJ_COUNT);
    unsigned int width, height, stride, source_stride;
    uint32_t *data, *source;
    cairo_surface_t *surf;
    width = obj[id].sprite_width;
    height = obj[id].sprite_height;

    surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
    data = xcairo_image_surface_get_argb32_data (surf);
    stride = xcairo_image_surface_get_argb32_stride (surf);
    xcairo_surface_flush (surf);

    source = xcairo_image_surface_get_argb32_data (objsheet);
    source_stride = xcairo_image_surface_get_argb32_stride (objsheet);

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
            uint32_t c32 = source[sj * source_stride + si];
            data[j * stride + i] = adjust_colorval (c32);
        }
    }
    xcairo_surface_mark_dirty (surf);
    return surf;
}

/****************************************************************/


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

DEFINE_VOID_FUNC_INT(objHide, obj_hide)
DEFINE_VOID_FUNC_INT(objShow, obj_show)
DEFINE_BOOL_FUNC_INT(objIsShown, obj_is_shown)
DEFINE_VOID_FUNC_INTx5_DOUBLEx2_BOOLx2_INT(objInit, obj_init)
DEFINE_VOID_FUNC_INTx3(objSetSpritesheetOrigin, obj_set_spritesheet_origin)
DEFINE_VOID_FUNC_INTx2_DOUBLEx4_INT(objSet, obj_set)
DEFINE_INT_FUNC_INT(objGetPriority, obj_get_priority)
DEFINE_VOID_FUNC_INT_DOUBLEx2(objSetRotationExpansion, obj_set_rotation_expansion)
DEFINE_VOID_FUNC_INT_DOUBLEx2(objSetPosition, obj_set_position)
DEFINE_VOID_FUNC_STRING(objUpdateSpritesheet, obj_update_spritesheet)

JSBool
objGetLocation(JSContext *cx, unsigned argc, jsval *vp)
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
    DECLARE_VOID_FUNC_INT (objHide, obj_hide)
    DECLARE_VOID_FUNC_INT (objShow, obj_show)
    DECLARE_BOOL_FUNC_INT (objIsShown, obj_is_shown)
    DECLARE_VOID_FUNC_INTx5_DOUBLEx2_BOOLx2_INT(objInit, obj_init)
    DECLARE_VOID_FUNC_INTx3(objSetSpritesheetOrigin, obj_set_spritesheet_origin)
    DECLARE_VOID_FUNC_INTx2_DOUBLEx4_INT(objSet, obj_set)
    DECLARE_INT_FUNC_INT(objGetPriority, obj_get_priority)
    DECLARE_VOID_FUNC_INT_DOUBLEx2(objSetRotationExpansion, obj_set_rotation_expansion)
    DECLARE_VOID_FUNC_INT_DOUBLEx2(objSetPosition, obj_set_position)
    DECLARE_VOID_FUNC_STRING(objUpdateSpritesheet, obj_update_spritesheet)
    JS_FS("objGetLocation", objGetLocation, 1, 0),
    JS_FS("objGetJsonString", objGetJsonString, 1, 0),
    JS_FS_END
};
