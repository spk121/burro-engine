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

Backdrop backdrop{};

Backdrop::Backdrop()
	: red{0},
	green{0},
	blue{0}
{
	
}

void Backdrop::set_color(uint8_t r, uint8_t g, uint8_t b)
{
    red = r;
    green = g;
    blue = b;
}

void Backdrop::get_color(uint8_t *r, uint8_t *g, uint8_t *b)
{
    *r = red;
    *g = green;
    *b = blue;
}

void Backdrop::draw(cairo_t *screen)
{
    uint8_t r8, g8, b8;

    get_color(&r8, &g8, &b8);
	
    double r {r8 / 255.0};
    double g {g8 / 255.0};
    double b {b8 / 255.0};
	
    xcairo_set_source_rgb (screen, r, g, b);
    xcairo_paint (screen);
}

static JSBool
BackdropSetColorFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    jsval* argv {JS_ARGV(cx, vp)};

    int r, g, b;

	if(!JS_ConvertArguments(cx, argc, argv, "iii", &r, &g, &b))
		return JS_FALSE;

    backdrop.set_color(static_cast<uint8_t>(r), static_cast<uint8_t>(g), static_cast<uint8_t>(b));

    JS_SET_RVAL(cx, vp, JSVAL_VOID);
	return JS_TRUE;
}

static JSBool
BackdropGetColorFunc(JSContext *cx, unsigned argc, jsval *vp)
{
    uint8_t r, g, b;
    backdrop.get_color(&r, &g, &b);

    JSObject *list { JS_NewArrayObject(cx, 3, NULL) };
    if (list) {
        JS::Value val;
        val = INT_TO_JSVAL(r);
        JS_SetElement(cx, list, 0, &val);
        val = INT_TO_JSVAL(g);
        JS_SetElement(cx, list, 1, &val);
        val = INT_TO_JSVAL(b);
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

