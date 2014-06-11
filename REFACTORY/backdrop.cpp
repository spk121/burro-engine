/* backdrop.c -- the lowest layer of the screen background.

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

#include "jsapi.hpp"
#include <cairo.h>
#include "backdrop.hpp"
#include "xcairo.hpp"

// The RGBA color displayed below all backgrounds and sprites.
static uint8_t red, green, blue;

static void
backdrop_set_color (uint8_t r, uint8_t g, uint8_t b)
{
    red = r;
    green = g;
    blue = b;
}

static void
backdrop_get_color (uint8_t *r, uint8_t *g, uint8_t *b)
{
    *r = red;
    *g = green;
    *b = blue;
}

extern "C" void
backdrop_draw (cairo_t *screen)
{
    uint8_t r8, g8, b8;
    double r, g, b;

    backdrop_get_color(&r8, &g8, &b8);
    r = (double) r8 / 255.0;
    g = (double) g8 / 255.0;
    b = (double) b8 / 255.0;
    xcairo_set_source_rgb (screen, r, g, b);
    xcairo_paint (screen);
}

JSBool
BackdropSetColorFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv = JS_ARGV(cx, vp);
    double r, g, b;

	if(!JS_ConvertArguments(cx, argc, argv, "ddd", &r, &g, &b))
		return JS_FALSE;

    backdrop_set_color(r, g, b);

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
	return JS_TRUE;
}

JSBool
BackdropGetColorFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    uint8_t r, g, b;
    backdrop_get_color(&r, &g, &b);

    JSObject *list = JS_NewArrayObject(cx, 3, NULL);
    if (list) {
        JS::Value val;
        val = INT_TO_JSVAL(red);
        JS_SetElement(cx, list, 0, &val);
        val = INT_TO_JSVAL(green);
        JS_SetElement(cx, list, 1, &val);
        val = INT_TO_JSVAL(blue);
        JS_SetElement(cx, list, 2, &val);
        JS_SET_RVAL(cx, vp, OBJECT_TO_JSVAL(list));
        return JS_TRUE;
    }
    return JS_FALSE;
}

JSFunctionSpec backdrop_functions[3] = {
    JS_FS("backdropGetColor",  BackdropGetColorFunc, 0, 0),
    JS_FS("backdropSetColor",  BackdropSetColorFunc, 3, 0),
    JS_FS_END
};

