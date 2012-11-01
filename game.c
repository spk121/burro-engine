#include <gtk/gtk.h>
#include <gio/gio.h>
#include "game.h"
#include "engine.h"
#include "tga.h"

void game_init ()
{
    int i, j, i2;
    targa_image_t tga;
    uint32_t c, r, g, b, a;
    GFile *fp = g_file_new_for_path("tower.tga");
    GFileInputStream *fis = g_file_read(fp, NULL, NULL);
    targa_parse_stream(G_INPUT_STREAM(fis),&tga);

    /* set background zero to this bitmap */
    e.main_bg[0].enable = 1;
    e.main_bg[0].mode = BG_MODE_INDEXED_BITMAP;
    e.main_bg[0].priority = 0;
    e.main_bg[0].center_x = 0.0;
    e.main_bg[0].center_y = 0.0;
    e.main_bg[0].center_i = 0;
    e.main_bg[0].center_j = 0;
    e.main_bg[0].expansion = 1.0;
    e.main_bg[0].rotation = 0.0;
    e.main_bg[0].bmp8.height_in_pixels = tga.header.image_height;
    e.main_bg[0].bmp8.width_in_pixels = tga.header.image_width;
    for (j = 0; j < tga.header.image_height; j++)
    {
        for (i = 0; i < tga.header.image_width; i++)
        {
        	e.main_bg[0].bmp8.bmp[j][i] = tga.data.image_data[j * tga.header.image_width + i];
        }
    }
    i = 0;
    for (c = 0; c < tga.header.color_map_length; c++)
    {
        if (tga.header.color_map_entry_size == 24 || tga.header.color_map_entry_size == 32)
        {
            r = tga.data.color_map_data[i++];
            g = tga.data.color_map_data[i++];
            b = tga.data.color_map_data[i++];
            a = 255;
        }
        if (tga.header.color_map_entry_size == 32)
            a = tga.data.color_map_data[i++];
        e.main_bg[0].bmp8.palette[c] = RGBA(r,g,b,a);
    }
}


int game_update_cb(engine_t *eng, double delta_t)
{
    static double total_t = 0.0;
    total_t += delta_t;

    eng->main_bg[0].center_x = 128.0;
    eng->main_bg[0].center_y = 128.0;
    eng->main_bg[0].center_i = 128;
    eng->main_bg[0].center_j = 128;
    eng->main_bg[0].rotation = total_t / 10.0;
    return 0;
}
