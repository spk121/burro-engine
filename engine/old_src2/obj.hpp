/* obj.hpp -- Object (sprite) functions

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

#ifndef BURRO_OBJ_H
#define BURRO_OBJ_H

#include <cairo.h>
#include "jsapi.hpp"

bool             obj_is_shown (int id);
cairo_surface_t *obj_render_to_cairo_surface (int id);
int              obj_get_priority (int id);
void             obj_get_location (int id, double *x, double *y, double *rotation_center_x,
                                   double *rotation_center_y, double *rotation, double *expansion);

extern JSFunctionSpec obj_functions[13];

#endif
