#ifndef BURRO_DRAW_H
#define BURRO_DRAW_H
void draw_initialize ();
cairo_surface_t *draw_get_main_screen_surface (void);
void draw (void);
void draw_finalize ();
#endif
