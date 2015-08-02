/** @file backdrop.h
    The lowest layer of the screen, which is a single color.
*/

#ifndef BURRO_BACKDROP_H
#define BURRO_BACKDROP_H

/** Index of the backdrop layers */
typedef enum backdrop_index_tag {
    BACKDROP_MAIN = 0,          /**< the backdrop of the main screen  */
    BACKDROP_SUB = 1,           /**< the backdrop of the sub screen  */
    BACKDROP_COUNT = 2,
} backdrop_index_t;

/** Get the color, as RGB of a screen's backdrop model */
void backdrop_get_color_rgb (backdrop_index_t id,
                             double *r, double *g, double *b);

/** Set the color, as an RGB24 integer, of a screen's backdrop model */
void backdrop_set_color (backdrop_index_t id, uint32_t c32);

/** Initialize the guile procedures for controlling the backdrops. */
void backdrop_init_guile_procedures (void);

#endif
