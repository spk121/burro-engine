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

// Console
#define FAST_BLINK_TIME 300	/* milliseconds */
#define SLOW_BLINK_TIME 500	/* milliseconds */
#define CONSOLE_ROWS 24
#define CONSOLE_COLS 80
#define CONSOLE_TAB 8		/* spaces per tab */
#define CONSOLE_VTAB 6		/* lines per vtab */
#define CONSOLE_OFFSET_X 0	/* pixels from left side of window */
#define CONSOLE_OFFSET_Y 0	/* pixels from top of window */
#define CONSOLE_EXPANSION 1.0	/* magnification of console w.r.t. screen */

// COLOR takes 4 bits
// Note that  DEFAULT has different meanings in the
// foreground and background cases.
#define COLOR_FG_DEFAULT        0b000000000000000
#define COLOR_FG_BLACK          0b000000000000001
#define COLOR_FG_RED            0b000000000000010
#define COLOR_FG_GREEN          0b000000000000011
#define COLOR_FG_YELLOW         0b000000000000100
#define COLOR_FG_BLUE           0b000000000000101
#define COLOR_FG_MAGENTA        0b000000000000110
#define COLOR_FG_CYAN           0b000000000000111
#define COLOR_FG_WHITE          0b000000000001000
#define COLOR_FG_TRANSPARENT    0b000000000001001
#define COLOR_FG_MASK           0b000000000001111
#define COLOR_FG_OFFSET         0

#define COLOR_BG_DEFAULT        0b000000000000000
#define COLOR_BG_BLACK          0b000000000010000
#define COLOR_BG_RED            0b000000000100000
#define COLOR_BG_GREEN          0b000000000110000
#define COLOR_BG_YELLOW         0b000000001000000
#define COLOR_BG_BLUE           0b000000001010000
#define COLOR_BG_MAGENTA        0b000000001100000
#define COLOR_BG_CYAN           0b000000001110000
#define COLOR_BG_WHITE          0b000000010000000
#define COLOR_BG_TRANSPARENT    0b000000010010000
#define COLOR_BG_MASK           0b000000011110000
#define COLOR_BG_OFFSET         4

#define INTENSITY_NORMAL        0b000000000000000
#define INTENSITY_FAINT         0b000000100000000
#define INTENSITY_BOLD          0b000001000000000
#define INTENSITY_MASK          0b000001100000000
#define INTENSITY_OFFSET        8

#define POLARITY_POSITIVE       0b000000000000000
#define POLARITY_NEGATIVE       0b000010000000000
#define POLARITY_MASK           0b000010000000000
#define POLARITY_OFFSET         10

#define BLINK_NONE              0b000000000000000
#define BLINK_SLOW              0b000100000000000
#define BLINK_FAST              0b001000000000000
#define BLINK_MASK              0b001100000000000
#define BLINK_OFFSET            11

#define UNDERLINE_NONE          0b000000000000000
#define UNDERLINE_SINGLY        0b010000000000000
#define UNDERLINE_DOUBLY        0b100000000000000
#define UNDERLINE_MASK          0b110000000000000
#define UNDERLINE_OFFSET        13 

// FRAME_RATE
#define UPDATE_RATE  (1000.0 / 60.0)	/* milliseconds between logic updates */
#define REFRESH_RATE (1000.0 / 60.0) /* milliseconds between frame redraws */

#ifdef __cplusplus
}
#endif

#endif
