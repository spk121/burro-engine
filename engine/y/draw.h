/*  draw.h

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
#ifndef BURRO_DRAW_H
#define BURRO_DRAW_H

#include "../x.h"

cairo_surface_t *draw_get_main_screen_surface (void);
cairo_surface_t *draw_get_sub_screen_surface (void);
void draw (void);
void draw_initialize (void);
void draw_finalize (void);

#endif
