/* console.hpp -- the top layer of the screen for text rendering

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

#ifndef BURRO_CONSOLE_H
#define BURRO_CONSOLE_H

#include <cairo.h>

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
void console_write_latin1_string(uint8_t *str);
void console_write_utf8_string(const char *str);
void console_write_wchar_string(const wchar_t *str,size_t len);

#endif
