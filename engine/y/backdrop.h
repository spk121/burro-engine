/** @file backdrop.h
    The lowest layer of the screen, which is a single color.
*/

#ifndef BURRO_BACKDROP_H
#define BURRO_BACKDROP_H

/** Get the color, as RGB of a screen's backdrop model */
void backdrop_get_color_rgb (double *r, double *g, double *b);

/** Set the color, as an RGB24 integer, of a screen's backdrop model */
void backdrop_set_color (uint32_t c32);

/** Initialize the guile procedures for controlling the backdrops. */
void backdrop_init_guile_procedures (void);

#endif
