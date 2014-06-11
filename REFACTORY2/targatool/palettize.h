#pragma once
#include <glib.h>
#include "targa.h"

// void palettize (guint8 *argb, gint n, gint bits_per_pixel, guint8 *index, guint8 *palette);
targa_error_t targa_convert_to_nonindexed (targa_image_t *t);
targa_error_t targa_convert_to_indexed (targa_image_t *t);
targa_error_t targa_convert_to_color (targa_image_t *t, color_format_t output_cf);




