#ifndef BURRO_DRAW_H
#define BURRO_DRAW_H
#include "../x.h"
cairo_surface_t *draw_get_main_screen_surface (void);
cairo_surface_t *draw_get_sub_screen_surface (void);
void draw (void);
void draw_initialize (void);
void draw_finalize (void);

#endif
