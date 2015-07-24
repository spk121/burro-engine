/* console.h -- a text rendering widget

   Copyright 2014, 2015 Michael L. Gran

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

#ifndef BURRO_CONSOLE_H
#define BURRO_CONSOLE_H

#include <stdint.h>
#include <stdbool.h>
#include "../x.h"

// Console
#define CONSOLE_ROWS 24
#define CONSOLE_COLS 80

enum console_color_index_tag
{
    CONSOLE_COLOR_DEFAULT = 0,
    CONSOLE_COLOR_BLACK = 1,
    CONSOLE_COLOR_RED = 2,
    CONSOLE_COLOR_GREEN = 3,
    CONSOLE_COLOR_YELLOW = 4,
    CONSOLE_COLOR_BLUE = 5,
    CONSOLE_COLOR_MAGENTA = 6,
    CONSOLE_COLOR_CYAN = 7,
    CONSOLE_COLOR_TRANSPARENT = 8
};

typedef enum console_color_index_tag console_color_index_t;

enum console_intensity_index_tag
{
    CONSOLE_INTENSITY_NORMAL = 0,
    CONSOLE_INTENSITY_FAINT = 1,
    CONSOLE_INTENSITY_BOLD = 2
};

typedef enum console_intensity_index_tag console_intensity_index_t;

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

bool console_is_visible (void);
void console_show (void);
void console_hide (void);

cairo_surface_t *console_render_to_cairo_surface(void);
void console_bell(void);
void console_delete_left(int n);
void console_delete_line_down(int n);
void console_delete_line_up(int n);
void console_delete_right(int n);
void console_erase_left(int n);
void console_erase_line(void);
void console_erase_page(void);
void console_erase_right(int n);
void console_erase_to_beginning_of_line(void);
void console_erase_to_beginning_of_page(void);
void console_erase_to_end_of_line(void);
void console_erase_to_end_of_page(void);
void console_insert_left(int n);
void console_insert_line_down(int n);
void console_insert_line_up(int n);
void console_insert_right(int n);
void console_move_down(int n);
void console_move_left(int n);
void console_move_right(int n);
void console_move_tab_left(int n);
void console_move_tab_right(int n);
void console_move_to(int r,int c);
void console_move_to_column(int n);
void console_move_to_row(int n);
void console_move_up(int n);
void console_move_vertical_tab_down(int n);
void console_move_vertical_tab_up(int n);
void console_reset(void);
void console_scroll_down(int n);
void console_scroll_left(int n);
void console_scroll_right(int n);
void console_scroll_up(int n);
void console_set_bgcolor(uint32_t c);
void console_set_blink(uint32_t c);
void console_set_cursor_visiblity(bool flag);
void console_set_default(void);
void console_set_fgcolor(uint32_t c);
void console_set_intensity(uint32_t c);
void console_set_polarity(uint32_t c);
void console_set_underline(uint32_t c);
void console_write_char(uint16_t codepoint,int irm,int hem,int simd,int home,int limit);
void console_write_latin1_string(const char *str);
void console_write_utf8_string(const char *str);
void console_write_ucs4_string(const uint32_t *str);
void console_write_wchar_string (const wchar_t *str, size_t len);
void console_test_pattern (void);

void console_init_guile_procedures (void);
#endif
