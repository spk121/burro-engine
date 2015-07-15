#ifndef BURRO_ENG_H
#define BURRO_ENG_H

#include "../x/xgtk.h"

#define BASE_WIDTH 256
#define BASE_HEIGHT 192
#define CIF_WIDTH 352
#define CIF_HEIGHT 288
#define QCIF_WIDTH 176
#define QCIF_HEIGHT 144
#define TERMINAL_WIDTH (8*80)
#define TERMINAL_HEIGHT (13*25)

enum eng_const_tag
{
  MAIN_SCREEN_WIDTH = CIF_WIDTH,
  MAIN_SCREEN_HEIGHT = CIF_HEIGHT,
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

unsigned int get_keyinput(void); 

#endif
