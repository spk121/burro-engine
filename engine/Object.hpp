/* Object.hpp -- Object (sprite) functions

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

#ifndef BURRO_OBJECT_H
#define BURRO_OBJECT_H

#include <cairo.h>
#include "jsapi.hpp"

struct Object_entry
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
};

extern Object_entry obj[MAIN_OBJ_COUNT];
extern cairo_surface_t *objsheet;

bool             obj_is_shown (int id);
cairo_surface_t *obj_render_to_cairo_surface (int id);
int              obj_get_priority (int id);
void             obj_get_location (int id, double *x, double *y, double *rotation_center_x,
                                   double *rotation_center_y, double *rotation, double *expansion);

extern JSFunctionSpec obj_functions[13];

#endif
