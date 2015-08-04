#include "../x.h"
#include "backdrop.h"

uint32_t bd[BACKDROP_COUNT];

void
backdrop_get_color_rgb (backdrop_index_t id, double *r, double *g, double *b)
{
    uint32_t c32 = bd[id];
    *r = ((double)((c32 & 0x00ff0000) >> 16)) / 255.0;
    *g = ((double)((c32 & 0x0000ff00) >> 8)) / 255.0;
    *b = ((double)((c32 & 0x000000ff))) / 255.0;
}

void backdrop_set_color (backdrop_index_t id, uint32_t color)
{
    bd[id] = color;
}

SCM_DEFINE (G_backdrop_set_color, "backdrop-set-color", 2, 0, 0,
            (SCM id, SCM argb32), "\
Sets the color of the lowest layer of the given screen to the color\n\
where COLOR is a 24-bit RGB colorval")
{
    backdrop_set_color (scm_to_int (id), scm_to_uint32 (argb32));
    return SCM_UNSPECIFIED;
}

void
backdrop_init_guile_procedures (void)
{
#include "backdrop.x"
    scm_c_export ("backdrop-set-color",
                  "BACKDROP_MAIN",
                  "BACKDROP_SUB",
                  NULL); 
}
