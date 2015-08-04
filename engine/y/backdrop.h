#ifndef BURRO_BACKDROP_H
#define BURRO_BACKDROP_H

typedef enum backdrop_index_tag {
    BACKDROP_MAIN = 0,
    BACKDROP_SUB = 1,
    BACKDROP_COUNT = 2,
} backdrop_index_t;

void backdrop_get_color_rgb (backdrop_index_t id,
                             double *r, double *g, double *b);
void backdrop_set_color (backdrop_index_t id, uint32_t c32);
void backdrop_init_guile_procedures (void);

#endif
