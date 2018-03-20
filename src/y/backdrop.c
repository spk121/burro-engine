/*  backdrop.c

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
#include "../x.h"
#include "backdrop.h"

uint32_t bd;

void
backdrop_get_color_rgb (double *r, double *g, double *b)
{
  uint32_t c32 = bd;
  *r = ((double)((c32 & 0x00ff0000) >> 16)) / 255.0;
  *g = ((double)((c32 & 0x0000ff00) >> 8)) / 255.0;
  *b = ((double)((c32 & 0x000000ff))) / 255.0;
}

void backdrop_set_color (uint32_t color)
{
  bd = color;
}

SCM_DEFINE (G_backdrop_get_color, "backdrop-get-color", 0, 0, 0,
            (void), "\
Returns the color of the lowest layer of the screen as a 24-bit\n\
RGB colorval")
{
  return scm_from_uint32(bd);
}


SCM_DEFINE (G_backdrop_set_color, "backdrop-set-color", 1, 0, 0,
            (SCM argb32), "\
Sets the color of the lowest layer of the screen to the color\n\
where COLOR is a 24-bit RGB colorval")
{
  backdrop_set_color (scm_to_uint32 (argb32));
  return SCM_UNSPECIFIED;
}

void
backdrop_init_guile_procedures (void)
{
#include "backdrop.x"
  scm_c_export ("backdrop-set-color",
		"backdrop-get-color",
		NULL);
}

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
