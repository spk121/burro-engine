#include <stdint.h>
#include <stdbool.h>
#include "../x.h"
#include "tilesheet.h"
#include "vram.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-conversion"

int tilesheet_width[6] = {
    [TILESHEET_SIZE_32x32] = 32,
    [TILESHEET_SIZE_128x128] = 128,
    [TILESHEET_SIZE_256x256] = 256,
    [TILESHEET_SIZE_512x256] = 512,
    [TILESHEET_SIZE_256x512] = 256,
    [TILESHEET_SIZE_512x512] = 512,
};

int tilesheet_height[6] = {
    [TILESHEET_SIZE_32x32] = 32,
    [TILESHEET_SIZE_128x128] = 128,
    [TILESHEET_SIZE_256x256] = 256,
    [TILESHEET_SIZE_512x256] = 256,
    [TILESHEET_SIZE_256x512] = 512,
    [TILESHEET_SIZE_512x512] = 512,
};

int tilesheet_size[6] = {
    [TILESHEET_SIZE_32x32] = 32*32,
    [TILESHEET_SIZE_128x128] = 128*128,
    [TILESHEET_SIZE_256x256] = 256*256,
    [TILESHEET_SIZE_512x256] = 512*256,
    [TILESHEET_SIZE_256x512] = 256*512,
    [TILESHEET_SIZE_512x512] = 512*512,
};

tilesheet_t ts[2];

int
tilesheet_get_height (tilesheet_index_t index)
{
    return tilesheet_height[ts[index].size];
}

int
tilesheet_get_width (tilesheet_index_t index)
{
    return tilesheet_width[ts[index].size];
}

int
tilesheet_get_height_in_tiles (tilesheet_index_t index)
{
    return tilesheet_height[ts[index].size] / TILE_HEIGHT;
}

int
tilesheet_get_width_in_tiles (tilesheet_index_t index)
{
    return tilesheet_width[ts[index].size] / TILE_WIDTH;
}


int
tilesheet_get_u32_size (tilesheet_index_t index)
{
    return tilesheet_size[ts[index].size];
} 

uint32_t **
tilesheet_get_u32_data (tilesheet_index_t index)
{
    return ts[index].data;
}

void
tilesheet_init (tilesheet_index_t index, tilesheet_size_t size,
                vram_bank_t bank)
{
    g_assert_cmpuint (tilesheet_size[size], >=,  vram_get_u32_size(bank));
    ts[index].bank = bank;
    ts[index].size = size;
    ts[index].storage = vram_get_u32_ptr(bank);
    g_free (ts[index].data);
    ts[index].data = g_new0(uint32_t *, tilesheet_height[size]);
    for (int i = 0; i < tilesheet_height[size]; i ++)
        ts[index].data[i] = ts[index].storage + i * tilesheet_width[size];
}

void tilesheet_set_data_from_file (tilesheet_index_t id, const char *filename)
{
    char *path = xg_find_data_file (filename);
    g_return_if_fail (path != NULL);
    GdkPixbuf *pb = xgdk_pixbuf_new_from_file (path);
    g_return_if_fail (pb != NULL);
    if (xgdk_pixbuf_is_argb32 (pb) == false)
    {
        xg_object_unref (pb);
        g_critical ("failed to load %s as an ARGB32 pixbuf", path);
        g_free (path);
    }
    else
    {
        int img_width, img_height, img_stride;
        int ts_width, ts_height;
        int width, height;

        xgdk_pixbuf_get_width_height_stride (pb,
                                            &img_width, &img_height,
                                            &img_stride);
        uint32_t *img_store = xgdk_pixbuf_get_argb32_pixels (pb);

        ts_width = tilesheet_width[ts[id].size];
        ts_height = tilesheet_height[ts[id].size];

        width = MIN(img_width, ts_width);
        height = MIN(img_height, ts_height);
        
        for (unsigned j = 0; j < height; j ++)
        {
            for (unsigned i = 0; i < width ; i ++)
            {
                ts[id].data[j][i] = img_store[j * img_stride + i];
            }
        }
        if (id == TILESHEET_MAIN)
            g_debug ("loaded pixbuf %s as bg main tilesheet", path);
        else
            g_debug ("loaded pixbuf %s as bg sub tilesheet", path);
        g_free (path);
        g_object_unref (pb);
    }
}

SCM_DEFINE (G_tilesheet_init, "tilesheet-init", 3, 0, 0,
            (SCM id, SCM size, SCM bank), "\
Set the size and VRAM storage of a given tilesheet")
{
    tilesheet_init (scm_to_int (id), scm_to_int (size), scm_to_int (bank));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_tilesheet_set_data_from_file, "tilesheet-set-data-from-file",
            2, 0, 0, (SCM id, SCM filename), "\
Copies the contents of an ARGB32 image into the tilesheet.  Note that the \n\
tilesheet's size and VRAM must first be set using 'tilesheet-init'")
{
    char *str = scm_to_locale_string (filename);
    tilesheet_set_data_from_file (scm_to_int (id), str);
    free (str);
    return SCM_UNSPECIFIED;
}

SCM_VARIABLE_INIT (G_TILESHEET_MAIN, "TILESHEET_MAIN",
                   scm_from_int (TILESHEET_MAIN));
SCM_VARIABLE_INIT (G_TILESHEET_SUB, "TILESHEET_SUB",
                   scm_from_int (TILESHEET_SUB));

SCM_VARIABLE_INIT (G_TILESHEET_SIZE_32x32, "TILESHEET_SIZE_32x32", scm_from_int (TILESHEET_SIZE_32x32));
SCM_VARIABLE_INIT (G_TILESHEET_SIZE_128x128, "TILESHEET_SIZE_128x128", scm_from_int (TILESHEET_SIZE_128x128));
SCM_VARIABLE_INIT (G_TILESHEET_SIZE_256x256, "TILESHEET_SIZE_256x256", scm_from_int (TILESHEET_SIZE_256x256));
SCM_VARIABLE_INIT (G_TILESHEET_SIZE_256x512, "TILESHEET_SIZE_256x512", scm_from_int (TILESHEET_SIZE_256x512));
SCM_VARIABLE_INIT (G_TILESHEET_SIZE_512x256, "TILESHEET_SIZE_512x256", scm_from_int (TILESHEET_SIZE_512x256));
SCM_VARIABLE_INIT (G_TILESHEET_SIZE_512x512, "TILESHEET_SIZE_512x512", scm_from_int (TILESHEET_SIZE_512x512));

void
tilesheet_init_guile_procedures (void)
{
#include "tilesheet.x"
    scm_c_export ("tilesheet-init",
                  "tilesheet-set-data-from-file",
                  "TILESHEET_MAIN",
                  "TILESHEET_SUB",
                  "TILESHEET_SIZE_32x32", 
                  "TILESHEET_SIZE_128x128", 
                  "TILESHEET_SIZE_256x256", 
                  "TILESHEET_SIZE_256x512", 
                  "TILESHEET_SIZE_512x256", 
                  "TILESHEET_SIZE_512x512", 
                  NULL);
}

#pragma GCC diagnostic pop

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
