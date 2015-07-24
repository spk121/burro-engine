/* console.c -- a text rendering widget

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

#include <memory.h>
#include <stdint.h>
#include <stdbool.h>
#include <wchar.h>
#include <stdlib.h>
#include <limits.h>

#include "../x.h"
#include "const.h"
#include "console.h"
#include "8x13.h"

#define COMPOSE(render,codepoint) (((uint32_t)(render) << 16)|(codepoint))
#define RENDERING(x) ((uint16_t)(((x) & 0xFFFF0000) >> 16))
#define CODEPOINT(x) ((uint16_t)((x) & 0x0000FFFF))

#define NUM_COLORS 10
#define NUM_INTENSITIES 3
#define FAST_BLINK_TIME 300	/* milliseconds */
#define SLOW_BLINK_TIME 500	/* milliseconds */
#define TAB 8		/* spaces per tab */
#define VTAB 6		/* lines per vtab */

static const uint32_t fg_palette[NUM_COLORS *
                                 NUM_INTENSITIES] = {
    /*                   normal     faint        bold        */
    /* default     */ 0xffcccc88, 0xff888888, 0xffffff88,
    /* black       */ 0xff000000, 0xff000000, 0xff000000,
    /* red         */ 0xffcc0000, 0xff880000, 0xffff0000,
    /* green       */ 0xff00cc00, 0xff008800, 0xff00ff00,
    /* yellow      */ 0xffcccc00, 0xff888800, 0xffffff00,
    /* blue        */ 0xff0000cc, 0xff000088, 0xff0000ff,
    /* magenta     */ 0xffcc00cc, 0xff880088, 0xffff00ff,
    /* cyan        */ 0xff00cccc, 0xff008888, 0xff00ffff,
    /* white       */ 0xffcccccc, 0xff888888, 0xffffffff,
    /* transparent */ 0x00000000, 0x00000000, 0x00000000,
};

static const uint32_t bg_palette[NUM_COLORS] = {
    /*                   background        */
    /* default     */ 0x00000000,
    /* black       */ 0xff000000,
    /* red         */ 0xffcc0000,
    /* green       */ 0xff00cc00,
    /* yellow      */ 0xffcccc00,
    /* blue        */ 0xff0000cc,
    /* magenta     */ 0xffcc00cc,
    /* cyan        */ 0xff00cccc,
    /* white       */ 0xffcccccc,
    /* transparent */ 0x00000000,
};

static int row, col;
static uint16_t rendition;
static uint32_t cells[CONSOLE_ROWS * CONSOLE_COLS];
static bool cursor_visible = TRUE;
static bool console_visible = TRUE;
GTimer *console_timer = NULL;

bool
console_is_visible ()
{
    return console_visible;
}


void
console_show ()
{
    console_visible = true;
}

void
console_hide ()
{
    console_visible = false;
}

void
console_reset (void)
{
    row = 0;
    col = 0;
    rendition = 0;
    cursor_visible = TRUE;
    memset (cells, 0, sizeof (cells));
    if (console_timer == NULL)
        console_timer = xg_timer_new ();
}

static void
set_rendition (uint16_t val, uint16_t mask)
{
    rendition &= ~mask;
    rendition |= (val & mask);
}

void
console_set_bgcolor (console_color_index_t c)
{
    switch (c)
    {
    case CONSOLE_COLOR_DEFAULT:
        set_rendition (COLOR_BG_DEFAULT, COLOR_BG_MASK);
        break;
    case CONSOLE_COLOR_BLACK:
        set_rendition (COLOR_BG_BLACK, COLOR_BG_MASK);
        break;
    case CONSOLE_COLOR_RED:
        set_rendition (COLOR_BG_RED, COLOR_BG_MASK);
        break;
    case CONSOLE_COLOR_GREEN:
        set_rendition (COLOR_BG_GREEN, COLOR_BG_MASK);
        break;
    case CONSOLE_COLOR_YELLOW:
        set_rendition (COLOR_BG_YELLOW, COLOR_BG_MASK);
        break;
    case CONSOLE_COLOR_BLUE:
        set_rendition (COLOR_BG_BLUE, COLOR_BG_MASK);
        break;
    case CONSOLE_COLOR_MAGENTA:
        set_rendition (COLOR_BG_MAGENTA, COLOR_BG_MASK);
        break;
    case CONSOLE_COLOR_CYAN:
        set_rendition (COLOR_BG_CYAN, COLOR_BG_MASK);
        break;
    case CONSOLE_COLOR_TRANSPARENT:
        set_rendition (COLOR_BG_TRANSPARENT, COLOR_BG_MASK);
        break;
    }
}

void
console_set_blink (uint32_t c)
{
    set_rendition (c, BLINK_MASK);
}

void
console_set_intensity (uint32_t c)
{
    set_rendition (c, INTENSITY_MASK);
}

void
console_set_fgcolor (console_color_index_t c)
{
    switch (c)
    {
    case CONSOLE_COLOR_DEFAULT:
        set_rendition (COLOR_FG_DEFAULT, COLOR_FG_MASK);
        break;
    case CONSOLE_COLOR_BLACK:
        set_rendition (COLOR_FG_BLACK, COLOR_FG_MASK);
        break;
    case CONSOLE_COLOR_RED:
        set_rendition (COLOR_FG_RED, COLOR_FG_MASK);
        break;
    case CONSOLE_COLOR_GREEN:
        set_rendition (COLOR_FG_GREEN, COLOR_FG_MASK);
        break;
    case CONSOLE_COLOR_YELLOW:
        set_rendition (COLOR_FG_YELLOW, COLOR_FG_MASK);
        break;
    case CONSOLE_COLOR_BLUE:
        set_rendition (COLOR_FG_BLUE, COLOR_FG_MASK);
        break;
    case CONSOLE_COLOR_MAGENTA:
        set_rendition (COLOR_FG_MAGENTA, COLOR_FG_MASK);
        break;
    case CONSOLE_COLOR_CYAN:
        set_rendition (COLOR_FG_CYAN, COLOR_FG_MASK);
        break;
    case CONSOLE_COLOR_TRANSPARENT:
        set_rendition (COLOR_FG_TRANSPARENT, COLOR_FG_MASK);
        break;
    }
}

void
console_set_polarity (uint32_t c)
{
    set_rendition (c, POLARITY_MASK);
}

void
console_set_underline (uint32_t c)
{
    set_rendition (c, UNDERLINE_MASK);
}

void
console_set_default (void)
{
    rendition = 0;
}

void
console_set_cursor_visiblity (bool flag)
{
    cursor_visible = flag;
}

// Deleting removes characters left of the cursor and then pulls the
// remaining characters in to fill the gap.
void
console_delete_left (int n)
{
    g_assert (n > 0);

    // Note that 'start' is inclusive, but 'end'
    // is exclusive, e.g., one past the end of the region.

    int delete_region_start = row * CONSOLE_COLS + col - (n - 1);
    // int delete_region_end = row * CONSOLE_COLS + col + 1;
    int source_region_start = row * CONSOLE_COLS;
    int source_region_end = delete_region_start;
    int destination_region_start = row * CONSOLE_COLS + n;
    int fill_region_start = source_region_start;
    int fill_region_end = destination_region_start;

    memmove (cells + destination_region_start,
             cells + source_region_start,
             sizeof (uint32_t) * (source_region_end - source_region_start));
    memset (cells + fill_region_start, 0,
            sizeof (uint32_t) * (fill_region_end - fill_region_start));
}

// Deleting removes characters right of the cursor and then pulls the
// remaining characters in to fill the gap.
void
console_delete_right (int n)
{
    g_assert (n > 0);

    // Note that 'start' is inclusive, but 'end'
    // is exclusive, e.g., one past the end of the region.

    int delete_region_start = row * CONSOLE_COLS + col;
    int delete_region_end = row * CONSOLE_COLS + col + n;
    int source_region_start = delete_region_end;
    int source_region_end = (row + 1) * CONSOLE_COLS;
    int destination_region_start = delete_region_start;
    int fill_region_start = source_region_end - n;
    int fill_region_end = source_region_end;

    memmove (cells + destination_region_start,
             cells + source_region_start,
             sizeof(uint32_t) * (source_region_end - source_region_start));
    memset (cells + fill_region_start, 0,
            sizeof(uint32_t) * (fill_region_end - fill_region_start));
}

// Deletes all off current line plus n-1 previous lines
void
console_delete_line_up (int n)
{
    g_assert (n > 0);

    // Note that 'start' is inclusive, but 'end'
    // is exclusive, e.g., one past the end of the region.

    int delete_region_start = (row - (n - 1)) * CONSOLE_COLS;
    // int delete_region_end = (row + 1) * CONSOLE_COLS;
    int source_region_start = 0;
    int source_region_end = delete_region_start;
    int destination_region_start = n * CONSOLE_COLS;
    int fill_region_start = source_region_start;
    int fill_region_end = destination_region_start;

    memmove (cells + destination_region_start,
             cells + source_region_start,
             sizeof(uint32_t) * (source_region_end - source_region_start));
    memset (cells + fill_region_start, 0,
            sizeof(uint32_t) * (fill_region_end - fill_region_start));
}

void
console_delete_line_down (int n)
{
    g_assert (n > 0);

    // Note that 'start' is inclusive, but 'end'
    // is exclusive, e.g., one past the end of the region.

    int delete_region_start = row * CONSOLE_COLS;
    int delete_region_end = (row + n) * CONSOLE_COLS;
    int source_region_start = delete_region_end;
    int source_region_end = CONSOLE_ROWS * CONSOLE_COLS;
    int destination_region_start = delete_region_start;
    int fill_region_start = (CONSOLE_ROWS - n) * CONSOLE_COLS;
    int fill_region_end = CONSOLE_ROWS * CONSOLE_COLS;

    // Delete the current line and the n-1 following lines.
    // by moving the rest of the buffer up to fill the gap.
    memmove (cells + destination_region_start,
             cells + source_region_start,
             sizeof(uint32_t) * (source_region_end - source_region_start));

    // Set everything beyond the copied region to the erased state.
    memset (cells + fill_region_start, 0,
            sizeof(uint32_t) * (fill_region_end - fill_region_start));
}

// Clears the current position and n-1 positions to the right
// without moving the cursor.
void
console_erase_left (int n)
{
    g_assert (n > 0);

    // Note that 'start' is inclusive, but 'end'
    // is exclusive, e.g., one past the end of the region.

    int erase_region_start = row * CONSOLE_COLS + col - (n - 1);
    int erase_region_end = row * CONSOLE_COLS + col + 1;

    memset (cells + erase_region_start, 0,
            sizeof(uint32_t) * (erase_region_end - erase_region_start));
}

void
console_erase_right (int n)
{
    g_assert (n > 0);

    if (col + n > CONSOLE_COLS)
        n = CONSOLE_COLS - col;
    // Note that 'start' is inclusive, but 'end'
    // is exclusive, e.g., one past the end of the region.

    int erase_region_start = row * CONSOLE_COLS + col;
    int erase_region_end = row * CONSOLE_COLS + col + n;

    memset (cells + erase_region_start, 0,
            sizeof(uint32_t) * (erase_region_end - erase_region_start));
}

void console_erase_to_beginning_of_line(void)
{
    int erase_region_start        = row * CONSOLE_COLS;
    int erase_region_end          = row * CONSOLE_COLS + col + 1;
    memset (cells + erase_region_start, 0,
            sizeof(uint32_t) * (erase_region_end - erase_region_start));
}

void console_erase_to_end_of_line(void)
{
    int erase_region_start        = row * CONSOLE_COLS + col;
    int erase_region_end          = (row + 1) * CONSOLE_COLS;
    memset (cells + erase_region_start, 0,
            sizeof(uint32_t) * (erase_region_end - erase_region_start));
}

void console_erase_line(void)
{
    int erase_region_start        = row * CONSOLE_COLS;
    int erase_region_end          = (row + 1) * CONSOLE_COLS;
    memset (cells + erase_region_start, 0,
            sizeof(uint32_t) * (erase_region_end - erase_region_start));
}

void console_erase_to_beginning_of_page(void)
{
    int erase_region_start        = 0;
    int erase_region_end          = row * CONSOLE_COLS + col + 1;
    memset (cells + erase_region_start, 0,
            sizeof(uint32_t) * (erase_region_end - erase_region_start));
}

void console_erase_to_end_of_page(void)
{
    int erase_region_start        = row * CONSOLE_COLS + col;
    int erase_region_end          = CONSOLE_ROWS * CONSOLE_COLS;
    memset (cells + erase_region_start, 0,
            sizeof(uint32_t) * (erase_region_end - erase_region_start));
}

void console_erase_page(void)
{
    int erase_region_start        = 0;
    int erase_region_end          = CONSOLE_ROWS * CONSOLE_COLS;
    memset (cells + erase_region_start, 0,
            sizeof(uint32_t) * (erase_region_end - erase_region_start));
}

// Inserting moves the text at the cursor and to the left of the cursor
// to the left.
void
console_insert_left (int n)
{
    g_assert (n > 0);

    // Note that 'start' is inclusive, but 'end'
    // is exclusive, e.g., one past the end of the region.
    int source_region_start = row * CONSOLE_COLS + n;
    int source_region_end   = row * CONSOLE_COLS + col + 1;
    int destination_region_start = row * CONSOLE_COLS;
    // int destination_region_end   = row * CONSOLE_COLS + col - (n - 1);
    int fill_region_start = row * CONSOLE_COLS + col - (n - 1);
    int fill_region_end   = row * CONSOLE_COLS + col + 1;

    memmove (cells + destination_region_start,
             cells + source_region_start,
             sizeof (uint32_t) * (source_region_end - source_region_start));
    memset (cells + fill_region_start, 0,
            sizeof (uint32_t) * (fill_region_end - fill_region_start));
}

// Inserting moves the text at the cursor and to the left of the cursor
// to the left.
void
console_insert_right (int n)
{
    g_assert (n > 0);

    // Note that 'start' is inclusive, but 'end'
    // is exclusive, e.g., one past the end of the region.
    int row_offset = row * CONSOLE_COLS;
    int source_region_start      = row_offset + col;
    int source_region_end        = row_offset + CONSOLE_COLS - n;
    int destination_region_start = row_offset + col + n;
    // int destination_region_end   = row_offset + CONSOLE_COLS;
    int fill_region_start        = row_offset + col;
    int fill_region_end          = row_offset + col + n;

    memmove (cells + destination_region_start,
             cells + source_region_start,
             sizeof (uint32_t) * (source_region_end - source_region_start));
    memset (cells + fill_region_start, 0,
            sizeof (uint32_t) * (fill_region_end - fill_region_start));
}

// Inserting moved the contents of the active line and the lines
// below it down n lines.
void
console_insert_line_down (int n)
{
    g_assert (n > 0);

    // Note that 'start' is inclusive, but 'end'
    // is exclusive, e.g., one past the end of the region.
    int source_region_start = row * CONSOLE_COLS;
    int source_region_end   = (CONSOLE_ROWS - n) * CONSOLE_COLS;
    int destination_region_start = (row + n) * CONSOLE_COLS;
    // int destination_region_end   = CONSOLE_ROWS * CONSOLE_COLS;
    int fill_region_start = row * CONSOLE_COLS;
    int fill_region_end   = (row + n) * CONSOLE_COLS;

    memmove (cells + destination_region_start,
             cells + source_region_start,
             sizeof (uint32_t) * (source_region_end - source_region_start));
    memset (cells + fill_region_start, 0,
            sizeof (uint32_t) * (fill_region_end - fill_region_start));
}

// Inserting moved the contents of the active line and the lines
// below it down n lines.
void
console_insert_line_up (int n)
{
    g_assert (n > 0);

    // Note that 'start' is inclusive, but 'end'
    // is exclusive, e.g., one past the end of the region.
    int source_region_start = n * CONSOLE_COLS;
    int source_region_end   = (row + 1) * CONSOLE_COLS;
    int destination_region_start = 0;
    // int destination_region_end   = (row - n + 1) * CONSOLE_COLS;
    int fill_region_start = (row - n) * CONSOLE_COLS;
    int fill_region_end   = (row + 1) * CONSOLE_COLS;

    memmove (cells + destination_region_start,
             cells + source_region_start,
             sizeof (uint32_t) * (source_region_end - source_region_start));
    memset (cells + fill_region_start, 0,
            sizeof (uint32_t) * (fill_region_end - fill_region_start));
}


void console_move_left(int n)
{
    g_assert (n > 0);

    col -= n;
    if (col < 0)
        col = 0;
}

void console_move_right(int n)
{
    g_assert (n > 0);

    col += n;
    if (col >= CONSOLE_COLS)
        col = CONSOLE_COLS - 1;
}

void console_move_up(int n)
{
    g_assert (n > 0);

    row -= n;
    if (row < 0)
        row = 0;
}

void console_move_down(int n)
{
    g_assert (n > 0);
    row += n;
    while (row >= CONSOLE_ROWS) {
        console_scroll_up(1);
        row --;
    }
}

void console_move_tab_left(int n)
{
    g_assert (n > 0);

    int c = col;
    int remainder = c % TAB;
    if (remainder == 0)
        c -= n * TAB;
    else
        c -= remainder + (n - 1) * TAB;
    if (c < 0)
        c = 0;
    col = c;
}

void console_move_tab_right(int n)
{
    g_assert (n > 0);

    int c = col;
    int remainder = TAB - (c % TAB);
    if (remainder == 0)
        c += n * TAB;
    else
        c += remainder + (n - 1) * TAB;
    if (c >= CONSOLE_COLS)
        c = CONSOLE_COLS - 1;
    col = c;
}

void console_move_vertical_tab_up(int n)
{
    g_assert (n > 0);

    int r = row;
    int remainder = r % VTAB;
    if (remainder == 0)
        r -= n * VTAB;
    else
        r -= remainder + (n - 1) * VTAB;
    if (r < 0)
        r = 0;
    row = r;
}

void console_move_vertical_tab_down(int n)
{
    g_assert (n > 0);

    int r = row;
    int remainder = VTAB - (r % VTAB);
    if (remainder == 0)
        r += n * VTAB;
    else
        r += remainder + (n - 1) * VTAB;
    if (r >= CONSOLE_ROWS)
        r = CONSOLE_ROWS - 1;
    row = r;
}
  
void console_move_to(int r, int c)
{
    g_assert (row >= 0);
    g_assert (col >= 0);

    if (r >= CONSOLE_ROWS)
        r = CONSOLE_ROWS - 1;
    if (c >= CONSOLE_COLS)
        c = CONSOLE_COLS - 1;
    row = r;
    col = c;
}

void console_move_to_row(int n)
{
    g_assert (n >= 0);

    if (n >= CONSOLE_ROWS)
        n = CONSOLE_ROWS - 1;
    row = n;
}

void console_move_to_column(int n)
{
    g_assert (n >= 0);
    if (n >= CONSOLE_COLS)
        n = CONSOLE_COLS - 1;
    col = n;
}

void console_scroll_left(int n)
{
    g_assert (n >= 0);

    // Since the 2D array is flattened into a 1D vector, we can just
    // pull the entire vector left, and then erase the end of each line.
    if (n >= CONSOLE_COLS) {
        console_erase_page ();
    }
    else {
        memmove(cells, cells + n,
                sizeof(uint32_t) * (CONSOLE_COLS * CONSOLE_ROWS - n));
        for (int r = 0; r < CONSOLE_ROWS; r ++) {
            int fill_region_start          = r * CONSOLE_COLS + CONSOLE_COLS - n;
            int fill_region_end            = r * CONSOLE_COLS + CONSOLE_COLS;
            // Set the end of the line to the erased state.
            memset(cells + fill_region_start, 0, 
                   sizeof(uint32_t) * (fill_region_end - fill_region_start));
        }
    }    
}


void console_scroll_right(int n)
{
    g_assert (n >= 0);

    // Since the 2D array is flattened into a 1D vector, we can just
    // push the entire vector right, and then erase the beginning of
    // each line.

    if (n >= CONSOLE_COLS) {
        console_erase_page ();
    }
    else {
        memmove(cells + n, cells,
                sizeof(uint32_t) * (CONSOLE_COLS * CONSOLE_ROWS - n));
        for (int r = 0; r < CONSOLE_ROWS; r ++) {
            int fill_region_start          = r * CONSOLE_COLS;
            int fill_region_end            = r * CONSOLE_COLS + n;

            // Set the beginning of each line to the erased state
            memset(cells + fill_region_start, 0, 
                   sizeof(uint32_t) * (fill_region_end - fill_region_start));
        }
    }    
}

void console_scroll_up(int n)
{
    g_assert (n >= 0);

    // Since the 2D array is flattened into a 1D vector, we can just
    // push the entire vector right, and then erase the beginning of
    // the vector

    if (n >= CONSOLE_ROWS) {
        console_erase_page ();
    }
    else {
        memmove(cells, cells + n * CONSOLE_COLS, 
                sizeof(uint32_t) * (CONSOLE_ROWS * CONSOLE_COLS - n * CONSOLE_COLS));
        memset(cells + (CONSOLE_ROWS - n) * CONSOLE_COLS, 0, 
               sizeof(uint32_t) * (n * CONSOLE_COLS));
    }
}

void console_scroll_down(int n)
{
    g_assert (n >= 0);

    // Since the 2D array is flattened into a 1D vector, we can just
    // pull the entire vector left, and then erase the end of the
    // vector.

    if (n >= CONSOLE_ROWS) {
        console_erase_page ();
    }
    else {
        memmove(cells + n * CONSOLE_COLS, cells,
                sizeof(uint32_t) * (CONSOLE_ROWS * CONSOLE_COLS - n * CONSOLE_COLS));
        memset(cells, 0, 
               sizeof(uint32_t) * (n * CONSOLE_COLS));
    }    
}


// Write the codepoint at the current location.  If IRM is 1 aka
// Insert Mode, this codepoint is being inserted in the row, and isn't
// overwriting a character.  If HEM is zero, insertion causes
// character motion to the right.  If HEM is 1, insertion pushes
// characters to the left.
// If SIMD is 0, the cursor moves right after a character is written.
// If SIMD is 1, the cursor moves left after the character is written.
// HOME and LIMIT bracket the valid character area.
void
console_write_char (uint16_t codepoint, int irm, int hem, int simd, int home, int limit)
{
    if (col < home && col > limit)
        return;

    if (irm == 1 && hem == 0 && col < limit)
    {
        int source_start = row * CONSOLE_COLS + col;
        int source_end = row * CONSOLE_COLS + limit;
        int destination_start = row * CONSOLE_COLS + col + 1;
        memmove (cells + destination_start,
                 cells + source_start, source_end - source_start);
    }
    else if (irm == 1 && hem == 1 && col > home)
    {
        int source_start = row * CONSOLE_COLS + home + 1;
        int source_end = row * CONSOLE_COLS + col + 1;
        int destination_start = row * CONSOLE_COLS + home;
        memmove (cells + destination_start,
                 cells + source_start, source_end - source_start);
    }

    cells[row * CONSOLE_COLS + col] = COMPOSE (rendition, codepoint);
    if (simd == 0)
    {
        col++;
        if (col > limit)
            col = limit;
    }
    else
    {
        col--;
        if (col < home)
            col = home;
    }
}

static glyph_fixed8x13_row_t *
get_narrow_glyph (int glyph_set, int codepoint,
                  int *bpd,	/* bits per dot: 1, 2, 3, 4 */
                  int *stride,		/* width of one font row in bytes */
                  int *width,		/* width of the glyph in bits */
                  int *height,		/* height of the glyph in bits */
                  int *xoffset,	/* top, left-hand corner X-offset in pixels */
                  int *yoffset		/* top left-hand corner Y-offset in pixels */
    )
{
    int i = 0;
    int count;
    int bits;
    
    /* FIXME: do a more efficient search */
    if (glyph_set == 0)
    {
        count = FIXED8x13_COUNT;
        bits = 1;
        
        while (i < count)
        {
            if (fixed8x13_glyphs[i].encoding == codepoint)
                break;
            i++;
        }
        if (i == count)
            return NULL;
        
        *bpd = bits;
        *stride = fixed8x13_glyphs[i].data.stride;
        *width = fixed8x13_glyphs[i].data.width;
        *height = fixed8x13_glyphs[i].data.height;
        *xoffset = fixed8x13_glyphs[i].data.xoffset;
        *yoffset = fixed8x13_glyphs[i].data.yoffset;
        return fixed8x13_glyphs[i].bitmap;
    }
    
    return NULL;
}

cairo_surface_t *
console_render_to_cairo_surface ()
{
    uint32_t timer, fast_blink_on, slow_blink_on;
    int width, height;
    cairo_surface_t *surf;
    uint32_t *data;
    int stride;
    
    int glyph_bpd, glyph_stride, glyph_width, glyph_height, glyph_xoffset, glyph_yoffset;
    uint8_t *glyph_bitmap;

    /* Convert time to integer milliseconds */
    timer = xg_timer_elapsed (console_timer) * 1000.0;
    fast_blink_on = (timer / FAST_BLINK_TIME) % 2;
    slow_blink_on = (timer / SLOW_BLINK_TIME) % 2;
    
    width = CONSOLE_COLS;
    height = CONSOLE_ROWS;
    surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                        width * FIXED8x13_MAXWIDTH,
                                        height * FIXED8x13_MAXHEIGHT);
    data = xcairo_image_surface_get_argb32_data (surf);
    stride = xcairo_image_surface_get_argb32_stride (surf);
    xcairo_surface_flush (surf);
    
    for (int r = 0; r < height; r++)
    {
        for (int c = 0; c < width; c++)
        {
            uint32_t cell = cells[r * CONSOLE_COLS + c];
            uint16_t rendering = RENDERING (cell);
            uint16_t codepoint = CODEPOINT (cell);
            uint16_t fg_color_index =
                (rendering & COLOR_FG_MASK) >> COLOR_FG_OFFSET;
            uint16_t bg_color_index =
                (rendering & COLOR_BG_MASK) >> COLOR_BG_OFFSET;
            uint16_t intensity_index =
                (rendering & INTENSITY_MASK) >> INTENSITY_OFFSET;
            uint16_t polarity = rendering & POLARITY_MASK;
            uint16_t blink = rendering & BLINK_MASK;
            uint16_t underline = rendering & UNDERLINE_MASK;
            
            uint32_t fg_argb =
                fg_palette[fg_color_index * NUM_INTENSITIES +
                           intensity_index];
            uint32_t bg_argb = bg_palette[bg_color_index];
            
            // Xor the inverse and blink states to see if we're inverse now.
            if ((polarity == POLARITY_NEGATIVE) !=
                (((blink == BLINK_FAST) && (fast_blink_on))
                 || ((blink == BLINK_SLOW) && (slow_blink_on))))
            {
                uint32_t temp_argb = fg_argb;
                fg_argb = bg_argb;
                bg_argb = temp_argb;
            }
            
            /* Swap again if we're the cursor and the cursor is visible */
            if (r == row && c == col && cursor_visible && slow_blink_on)
            {
                uint32_t temp_argb = fg_argb;
                fg_argb = bg_argb;
                bg_argb = temp_argb;
            }

            glyph_bitmap = get_narrow_glyph (0,
                                             codepoint,
                                             &glyph_bpd, &glyph_stride,
                                             &glyph_width, &glyph_height,
                                             &glyph_xoffset, &glyph_yoffset);

            if (glyph_bitmap != NULL)
            {
                for (int j = 0; j < glyph_height; j++)
                {
                    for (int i = 0; i < glyph_width; i++)
                    {
                        uint32_t pixel_argb;
						
                        if ((underline == UNDERLINE_SINGLY
                             || underline == UNDERLINE_DOUBLY)
                            && (j == glyph_height - 1))
                            pixel_argb = fg_argb;
                        else if ((underline == UNDERLINE_DOUBLY) &&
                                 (j == glyph_height - 2))
                            pixel_argb = fg_argb;
                        else if (glyph_bitmap
                                 && glyph_bitmap[j] & (1 << (glyph_width - i - 1)))
                            pixel_argb = fg_argb;
                        else if (glyph_bitmap
                                 && (i > 0)
                                 && ((rendering & INTENSITY_MASK) == INTENSITY_BOLD)
                                 && (glyph_bitmap[j] & (1 << (glyph_width - i - 2))))
                            pixel_argb = fg_argb;
                        else
                            pixel_argb = bg_argb;
						
                        data[(r * FIXED8x13_MAXHEIGHT + j) * stride
                             + c * FIXED8x13_MAXWIDTH + i] = pixel_argb;
                    }
                }
            }
            else {
				
                for (int j = 0; j < FIXED8x13_MAXHEIGHT; j++)
                {
                    for (int i = 0; i < FIXED8x13_MAXWIDTH; i++)
                    {
                        uint32_t pixel_argb;
						
                        if ((underline == UNDERLINE_SINGLY
                             || underline == UNDERLINE_DOUBLY)
                            && (j == glyph_height - 1))
                            pixel_argb = fg_argb;
                        else if ((underline == UNDERLINE_DOUBLY) &&
                                 (j == glyph_height - 2))
                            pixel_argb = fg_argb;
                        else
                            pixel_argb = bg_argb;
						
                        data[(r * FIXED8x13_MAXHEIGHT + j) * stride
                             + c * FIXED8x13_MAXWIDTH + i] = pixel_argb;
                    }
                }
            }
            xcairo_surface_mark_dirty (surf);
        }
    }
    return surf;
}

void
console_bell (void)
{
    // Ring a bell, somehow
}

void
console_write_ucs4_string (const uint32_t *str)
{
    size_t i = 0;
    while (str[i] != 0)
    {
        if (col == CONSOLE_COLS - 1)
        {
            row ++;
            col = 0;
            if (row == CONSOLE_ROWS)
            {
                console_scroll_up (1);
                row --;
            }
        }
        console_write_char (str[i], 0, 0, 0, 0, CONSOLE_COLS - 1);
        i = i + 1;
    }
}

void
console_write_latin1_string (const char * str)
{
    size_t i = 0;
    while (str[i] != '\0')
    {
        if (col == CONSOLE_COLS - 1)
        {
            row ++;
            col = 0;
            if (row == CONSOLE_ROWS)
            {
                console_scroll_up (1);
                row --;
            }
        }
        console_write_char ((uint8_t)(str[i]), 0, 0, 0, 0, CONSOLE_COLS - 1);
        i = i + 1;
    }
}

void
console_write_wchar_string (const wchar_t *str, size_t len)
{
    for (size_t i = 0; i < len; i ++) {
        if (col == CONSOLE_COLS - 1) {
            row ++;
            col = 0;
            if (row == CONSOLE_ROWS) {
                console_scroll_up (1);
                row --;
            }
        }
        console_write_char (str[i], 0, 0, 0, 0, CONSOLE_COLS - 1);
    }
}

void
console_write_utf8_string (const char *str)
{
    g_return_if_fail (str != NULL);
    
    uint32_t *ucs4 = xg_utf8_to_ucs4 (str);

    console_write_ucs4_string (ucs4);
    g_free (ucs4);
}

void
console_test_pattern (void)
{
    for (int i = 0; i < FIXED8x13_COUNT; i++)
    {
        console_write_char (fixed8x13_glyphs[i].encoding, 0, 0, 0, 0, CONSOLE_COLS - 1);
        if (col == CONSOLE_COLS - 1)
        {
            row ++;
            col = 0;
        }
    }
    row++;
    col = 0;

    row++;
    col = 0;
    console_set_bgcolor (CONSOLE_COLOR_DEFAULT);
    console_write_latin1_string ("DEFAULT");
    console_set_bgcolor (CONSOLE_COLOR_BLACK);
    console_write_latin1_string ("BLACK");
    console_set_bgcolor (CONSOLE_COLOR_RED);
    console_write_latin1_string ("RED");
    console_set_bgcolor (CONSOLE_COLOR_GREEN);
    console_write_latin1_string ("GREEN");
    console_set_bgcolor (CONSOLE_COLOR_YELLOW);
    console_write_latin1_string ("YELLOW");
    console_set_bgcolor (CONSOLE_COLOR_BLUE);
    console_write_latin1_string ("BLUE");
    console_set_bgcolor (CONSOLE_COLOR_MAGENTA);
    console_write_latin1_string ("MAGENTA");
    console_set_bgcolor (CONSOLE_COLOR_CYAN);
    console_write_latin1_string ("CYAN");
    console_set_bgcolor (CONSOLE_COLOR_WHITE);
    console_write_latin1_string ("WHITE");
    console_set_bgcolor (CONSOLE_COLOR_TRANSPARENT);
    console_write_latin1_string ("CLEAR");

    for (int i = 0; i < 3; i++)
    {
        row++;
        col = 0;
        console_set_fgcolor (CONSOLE_COLOR_DEFAULT);
        if (i == 0)
        {
            console_set_intensity (INTENSITY_BOLD);
            console_write_latin1_string ("BOLD   ");
        }
        else if (i == 1)
        {
            console_set_intensity (INTENSITY_FAINT);
            console_write_latin1_string ("FAINT  ");
        }
        else if (i == 2)
        {
            console_set_intensity (INTENSITY_NORMAL);
            console_write_latin1_string ("NORMAL ");
        }
        console_set_fgcolor (CONSOLE_COLOR_BLACK);
        console_write_latin1_string ("BLACK");
        console_set_fgcolor (CONSOLE_COLOR_RED);
        console_write_latin1_string ("RED");
        console_set_fgcolor (CONSOLE_COLOR_GREEN);
        console_write_latin1_string ("GREEN");
        console_set_fgcolor (CONSOLE_COLOR_YELLOW);
        console_write_latin1_string ("YELLOW");
        console_set_fgcolor (CONSOLE_COLOR_BLUE);
        console_write_latin1_string ("BLUE");
        console_set_fgcolor (CONSOLE_COLOR_MAGENTA);
        console_write_latin1_string ("MAGENTA");
        console_set_fgcolor (CONSOLE_COLOR_CYAN);
        console_write_latin1_string ("CYAN");
        console_set_fgcolor (CONSOLE_COLOR_WHITE);
        console_write_latin1_string ("WHITE");
        console_set_fgcolor (CONSOLE_COLOR_TRANSPARENT);
        console_write_latin1_string ("CLEAR");
    }

    row++;
    col = 0;
    console_set_fgcolor (CONSOLE_COLOR_DEFAULT);
    console_set_bgcolor (CONSOLE_COLOR_DEFAULT);
    console_set_blink (BLINK_NONE);
    console_write_latin1_string ("STEADY");
    console_set_blink (BLINK_SLOW);
    console_write_latin1_string ("BLINK1");
    console_set_blink (BLINK_FAST);
    console_write_latin1_string ("BLINK2");
    console_set_blink (BLINK_NONE);

    console_set_underline (UNDERLINE_NONE);
    console_write_latin1_string ("NORMAL");
    console_set_underline (UNDERLINE_SINGLY);
    console_write_latin1_string ("UNDERLINE1");
    console_set_underline (UNDERLINE_DOUBLY);
    console_write_latin1_string ("UNDERLINE2");
    console_set_underline (UNDERLINE_NONE);

    row++;
    col = 0;
    console_set_polarity (POLARITY_POSITIVE);
    console_write_latin1_string ("POSITIVE");
    console_set_polarity (POLARITY_NEGATIVE);
    console_write_latin1_string ("NEGATIVE");
    console_set_polarity (POLARITY_POSITIVE);

}

////////////////////////////////////////////////////////////////

SCM_DEFINE (G_console_hide, "console-hide", 0, 0, 0, (void), "")
{
    console_hide ();
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_console_reset, "console-reset", 0, 0, 0, (void), "")
{
    console_reset ();
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_console_set_bgcolor, "console-set-bgcolor", 1, 0, 0, (SCM color), "")
{
    console_set_bgcolor (scm_to_int (color));
}

SCM_DEFINE (G_console_set_fgcolor, "console-set-fgcolor", 1, 0, 0, (SCM color), "")
{
    console_set_fgcolor (scm_to_int (color));
}

SCM_DEFINE (G_console_show, "console-show", 0, 0, 0, (void), "")
{
    console_show ();
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_console_visible_p, "console-visible?", 0, 0, 0, (void), "\
Returns #t if the console is being drawn.")
{
    return scm_from_bool (console_is_visible());
}

void
console_init_guile_procedures (void)
{
#include "console.x"
    scm_c_export ("console-hide",
                  "console-reset",
                  "console-set-bgcolor",
                  "console-set-fgcolor",
                  "console-show",
                  "console-visible?",
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
