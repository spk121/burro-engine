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


enum console_blink_index_tag
{
    CONSOLE_BLINK_NONE = 0,
    CONSOLE_BLINK_SLOW = 1,
    CONSOLE_BLINK_FAST = 2,
};

typedef enum console_blink_index_tag console_blink_index_t;

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
    CONSOLE_COLOR_WHITE = 8,
    CONSOLE_COLOR_TRANSPARENT = 9
};

typedef enum console_color_index_tag console_color_index_t;

enum console_intensity_index_tag
{
    CONSOLE_INTENSITY_NORMAL = 0,
    CONSOLE_INTENSITY_FAINT = 1,
    CONSOLE_INTENSITY_BOLD = 2
};

typedef enum console_intensity_index_tag console_intensity_index_t;

enum console_polarity_index_tag
{
    CONSOLE_POLARITY_POSITIVE = 0,
    CONSOLE_POLARITY_NEGATIVE = 1,
};

typedef enum console_polarity_index_tag console_polarity_index_t;

enum console_underline_index_tag
{
    CONSOLE_UNDERLINE_NONE = 0,
    CONSOLE_UNDERLINE_SINGLY = 1,
    CONSOLE_UNDERLINE_DOUBLY = 2
};

typedef enum console_underline_index_tag console_underline_index_t;

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
void console_set_bgcolor(console_color_index_t c);
void console_set_blink(console_blink_index_t c);
void console_set_cursor_visibility(bool flag);
void console_set_default(void);
void console_set_fgcolor(console_color_index_t c);
void console_set_intensity(console_intensity_index_t c);
void console_set_polarity(console_polarity_index_t c);
void console_set_underline(uint32_t c);
void console_write_char(uint16_t codepoint,int irm,int hem,int simd,int home,int limit);
void console_write_latin1_string(const char *str);
void console_write_utf8_string(const char *str);
void console_write_ucs4_string(const uint32_t *str);
void console_write_wchar_string (const wchar_t *str, size_t len);
void console_test_pattern (void);

void console_init_guile_procedures (void);
#endif
