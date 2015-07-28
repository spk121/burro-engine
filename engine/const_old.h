/* const.h -- the static sizes and other constants

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


#ifndef BURRO_CONST_H
#define BURRO_CONST_H

#ifdef __cplusplus
extern "C" {
#endif

// Main screen
#define MAIN_SCREEN_WIDTH 640
#define MAIN_SCREEN_HEIGHT 312
#define MAIN_SCREEN_MAGNIFICATION 1

// Backgrounds
#define MAIN_BACKGROUNDS_COUNT 4
#define BG_COLOR16_BLACK 0
#define PRIORITY_COUNT 4
// -- Map-and-tile backgrounds
#define MAP_HEIGHT 512
#define MAP_WIDTH 512
#define TILESHEET_HEIGHT 256
#define TILESHEET_WIDTH 256
#define TILE_HEIGHT 8
#define TILE_WIDTH (8)
#define TILESHEET_HEIGHT_IN_TILES (256/8)
#define TILESHEET_WIDTH_IN_TILES (256/8)
#define PALETTE_COLORS_COUNT 256
// -- Bitmap backgrounds
#define BMP8_HEIGHT 512
#define BMP8_WIDTH 512
#define BMP16_HEIGHT 512
#define BMP16_WIDTH 512

// Sprites
#define MAIN_OBJ_COUNT 128
#define OBJSHEET_HEIGHT 256
#define OBJSHEET_WIDTH 256
#define OBJSHEET_PALETTE_COLORS_COUNT 512


// FRAME_RATE
#define UPDATE_RATE  (1000.0 / 60.0)	/* milliseconds between logic updates */
#define REFRESH_RATE (1000.0 / 60.0) /* milliseconds between frame redraws */

#ifdef __cplusplus
}
#endif

#endif
