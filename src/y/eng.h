/*  eng.c

    Copyright (C) 2013, 2014, 2018   Michael L. Gran
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
#ifndef BURRO_ENG_H
#define BURRO_ENG_H

#include "../x.h"

// Most Playstation 1 games were QVGA
#define PS1_WIDTH 320
#define PS1_HEIGHT 240

// Nintendo DS2 resolution
#define DS2_WIDTH 256
#define DS2_HEIGHT 192

// CIF and QCIF are very old-school analog video format
#define CIF_WIDTH 352
#define CIF_HEIGHT 288
#define QCIF_WIDTH 176
#define QCIF_HEIGHT 144

#define GAME_BOY_WIDTH 160
#define GAME_BOY_HEIGHT 144

#define VGA_WIDTH 640
#define VGA_HEIGHT 480

//#define SMALL_FONT
#undef SMALL_FONT
#ifdef SMALL_FONT
#define FONT_WIDTH 6
#define FONT_HEIGHT 9
#else
#define FONT_WIDTH 8
#define FONT_HEIGHT 13
#endif

#define CONSOLE_ROWS (VGA_HEIGHT/FONT_HEIGHT)
#define CONSOLE_COLS (VGA_WIDTH/FONT_WIDTH)

#define TERMINAL_WIDTH (FONT_WIDTH * CONSOLE_COLS)
#define TERMINAL_HEIGHT (FONT_HEIGHT * CONSOLE_ROWS)

enum eng_const_tag
{
  MAIN_SCREEN_WIDTH = VGA_WIDTH,
  MAIN_SCREEN_HEIGHT = VGA_HEIGHT,
  MAIN_SCREEN_MAGNIFICATION = 1,
  PRIORITY_COUNT = 4,
};

GtkWidget *eng_initialize (void);
gboolean eng_is_blank (void);
void eng_blank (void);
void eng_unblank (void);
gboolean eng_is_colorswap (void);
void eng_colorswap (void);
void eng_uncolorswap (void);
void eng_present (void);
gdouble eng_get_brightness (void);
void eng_set_brightness (gdouble brightness);

unsigned int eng_get_keyinput(void);
void eng_emit_shutdown(void);
void eng_init_guile_procedures(void);

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
