#ifndef ENG_DRAW_H_INCLUDED
#define ENG_DRAW_H_INCLUDED

void initialize_video(void);
void fini_draw (void);
void draw (void);


extern cairo_t *main_screen_context;
extern cairo_surface_t *main_screen_surface;

extern cairo_t *sub_screen_context;
extern cairo_surface_t *sub_screen_surface;
#endif // ENG_DRAW_H_INCLUDED
