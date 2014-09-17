/* backdrop.hpp -- the lowest layer of the screen background.

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

#ifndef BURRO_BACKDROP_H
#define BURRO_BACKDROP_H

#include <cstdint>
#include <cairo.h>

#include "jsapi.hpp"

class Backdrop
{
private:
	uint8_t red, green, blue;

public:
	Backdrop();
	void set_color(uint8_t r, uint8_t g, uint8_t b);
	void get_color(uint8_t *r, uint8_t *g, uint8_t *b);
	void draw(cairo_t *screen);
};

extern Backdrop backdrop;
extern JSFunctionSpec backdrop_functions[3];

#endif
