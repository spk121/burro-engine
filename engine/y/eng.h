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

//#define SMALL_FONT
#undef SMALL_FONT
#ifdef SMALL_FONT
#define FONT_WIDTH 6
#define FONT_HEIGHT 9
#else
#define FONT_WIDTH 8
#define FONT_HEIGHT 13
#endif

#define CONSOLE_ROWS ((DS2_HEIGHT*2)/FONT_HEIGHT)
#define CONSOLE_COLS ((DS2_WIDTH*2)/FONT_WIDTH)

#define TERMINAL_WIDTH (FONT_WIDTH * CONSOLE_COLS)
#define TERMINAL_HEIGHT (FONT_HEIGHT * CONSOLE_ROWS)

enum eng_const_tag
{
  MAIN_SCREEN_WIDTH = DS2_WIDTH * 2,
  MAIN_SCREEN_HEIGHT = DS2_HEIGHT * 2,
  SUB_SCREEN_WIDTH = TERMINAL_WIDTH,
  SUB_SCREEN_HEIGHT = TERMINAL_HEIGHT,
  MAIN_SCREEN_MAGNIFICATION = 1,
  SUB_SCREEN_MAGNIFICATION = 1,
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
void eng_init_guile_procedures(void);

#endif
