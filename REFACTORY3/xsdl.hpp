/* xsdl2.hpp -- helper functions for the SDL2 library

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

#ifndef BURRO_XSDL_H
#define BURRO_XSDL_H

#include <SDL.h>
#include <stdint.h>

void        xsdl_init_file_io (void);
const char* xsdl_get_data_path (void);
uint8_t     xsdl_read_uint8 (SDL_RWops *context);
size_t      xsdl_read_char_array (char *ptr, size_t len, SDL_RWops *istream);
size_t      xsdl_read_uint8_array (uint8_t *ptr, size_t len, SDL_RWops *istream);
size_t      xsdl_read_rle_array (void *data, size_t size, size_t count, SDL_RWops* istream);
SDL_RWops*  xSDL_RWFromDataFile(const char *file, const char *mode);
int64_t     xSDL_RWseek(SDL_RWops* context, int64_t offset, int whence);
void        xSDL_Init_or_die (uint32_t flags);

#endif
