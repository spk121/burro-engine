#include "../x.h"
#include "backdrop.h"

uint32_t bd;

void
backdrop_get_color_rgb (double *r, double *g, double *b)
{
    uint32_t c32 = bd;
    *r = ((double)((c32 & 0x00ff0000) >> 16)) / 255.0;
    *g = ((double)((c32 & 0x0000ff00) >> 8)) / 255.0;
    *b = ((double)((c32 & 0x000000ff))) / 255.0;
}

void backdrop_set_color (uint32_t color)
{
    bd = color;
}

SCM_DEFINE (G_backdrop_set_color, "backdrop-set-color", 1, 0, 0,
            (SCM argb32), "\
Sets the color of the lowest layer of the screen to the color\n\
where COLOR is a 24-bit RGB colorval")
{
    backdrop_set_color (scm_to_uint32 (argb32));
    return SCM_UNSPECIFIED;
}

void
backdrop_init_guile_procedures (void)
{
#include "backdrop.x"
    scm_c_export ("backdrop-set-color",
                  NULL); 
}
