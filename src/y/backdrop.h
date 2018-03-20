/*  backdrop.h

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

/** @file backdrop.h
    The lowest layer of the screen, which is a single color.
*/

#ifndef BURRO_BACKDROP_H
#define BURRO_BACKDROP_H

/** Get the color, as RGB of a screen's backdrop model */
void backdrop_get_color_rgb (double *r, double *g, double *b);

/** Set the color, as an RGB24 integer, of a screen's backdrop model */
void backdrop_set_color (uint32_t c32);

/** Initialize the guile procedures for controlling the backdrops. */
void backdrop_init_guile_procedures (void);

#endif


/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
