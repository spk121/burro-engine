/* tga.hpp -- Targa functions

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

#ifndef BURRO_TGA_H
#define BURRO_TGA_H

typedef struct tga_image_tag tga_image_t;

typedef enum tga_hflip_tag
{
    LEFT_TO_RIGHT = 0,
    RIGHT_TO_LEFT = 1
} tga_hflip_t;

typedef enum tga_vflip_tag
{
    TOP_TO_BOTTOM = 1,
    BOTTOM_TO_TOP = 0
} tga_vflip_t;

bool         tga_has_image (const tga_image_t *t);
bool         tga_has_palette (const tga_image_t *t);
void         tga_get_image_dimensions (const tga_image_t *t, unsigned int *width, unsigned int *height);
void         tga_get_image_orientation (const tga_image_t *t, tga_hflip_t *hflip, tga_vflip_t *vflip);
unsigned int tga_get_color_map_length (const tga_image_t *t);
unsigned int tga_get_color_map_first_index (const tga_image_t *t);
uint16_t*    tga_get_image_data_u16_ptr (const tga_image_t *t);
uint8_t*     tga_get_image_data_u8_ptr (const tga_image_t *t);
uint16_t*    tga_get_color_map_data_u16_ptr (const tga_image_t *t);
tga_image_t* tga_load_from_resource (const char *resource);
void         tga_free (tga_image_t *t);

#endif
