#ifndef __BURRO_CANVAS_H
#define __BURRO_CANVAS_H

#include <stdint.h>
#include <gtk/gtk.h>
#include "burro_canvas_vram.h"

#define BURRO_TYPE_CANVAS (burro_canvas_get_type ())
G_DECLARE_FINAL_TYPE (BurroCanvas, burro_canvas, BURRO, CANVAS, GtkDrawingArea)

#define BURRO_CANVAS_WIDTH 512
#define BURRO_CANVAS_HEIGHT 384

BurroCanvas *burro_canvas_new ();

void burro_canvas_init_guile_procedures ();

#if 0
void burro_canvas_backdrop_get_color_rgb (BurroCanvas *canvas, double *r, double *g, double *b);
void burro_canvas_backdrop_set_color (BurroCanvas *canvas, uint32_t color);

void burro_canvas_hide_backgrounds_and_objects (BurroCanvas *canvas);
void burro_canvas_show_backgrounds_and_objects (BurroCanvas *canvas);
gboolean burro_canvas_backgrounds_and_objects_are_visible (BurroCanvas *canvas);
#endif

#endif