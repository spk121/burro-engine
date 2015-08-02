#include <stdint.h>
#include <stdbool.h>
#include "../x.h"
#include "spritesheet.h"
#include "vram.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-conversion"

int spritesheet_width[6] = {
    [SPRITESHEET_SIZE_32x32] = 32,
    [SPRITESHEET_SIZE_128x128] = 128,
    [SPRITESHEET_SIZE_256x256] = 256,
    [SPRITESHEET_SIZE_512x256] = 512,
    [SPRITESHEET_SIZE_256x512] = 256,
    [SPRITESHEET_SIZE_512x512] = 512,
};

int spritesheet_height[6] = {
    [SPRITESHEET_SIZE_32x32] = 32,
    [SPRITESHEET_SIZE_128x128] = 128,
    [SPRITESHEET_SIZE_256x256] = 256,
    [SPRITESHEET_SIZE_512x256] = 256,
    [SPRITESHEET_SIZE_256x512] = 512,
    [SPRITESHEET_SIZE_512x512] = 512,
};

int spritesheet_size[6] = {
    [SPRITESHEET_SIZE_32x32] = 32*32,
    [SPRITESHEET_SIZE_128x128] = 128*128,
    [SPRITESHEET_SIZE_256x256] = 256*256,
    [SPRITESHEET_SIZE_512x256] = 512*256,
    [SPRITESHEET_SIZE_256x512] = 256*512,
    [SPRITESHEET_SIZE_512x512] = 512*512,
};

spritesheet_t ts[2];

int
spritesheet_get_height (spritesheet_index_t index)
{
    return spritesheet_height[ts[index].size];
}

int
spritesheet_get_width (spritesheet_index_t index)
{
    return spritesheet_width[ts[index].size];
}

int
spritesheet_get_height_in_sprites (spritesheet_index_t index)
{
    return spritesheet_height[ts[index].size] / SPRITE_HEIGHT;
}

int
spritesheet_get_width_in_sprites (spritesheet_index_t index)
{
    return spritesheet_width[ts[index].size] / SPRITE_WIDTH;
}


int
spritesheet_get_u32_size (spritesheet_index_t index)
{
    return spritesheet_size[ts[index].size];
} 

uint32_t **
spritesheet_get_u32_data (spritesheet_index_t index)
{
    return ts[index].data;
}

void
spritesheet_init (spritesheet_index_t index, spritesheet_size_t size,
                vram_bank_t bank)
{
    g_assert_cmpuint (spritesheet_size[size], >=,  vram_get_u32_size(bank));
    ts[index].bank = bank;
    ts[index].size = size;
    ts[index].storage = vram_get_u32_ptr(bank);
    g_free (ts[index].data);
    ts[index].data = g_new0(uint32_t *, spritesheet_height[size]);
    for (int i = 0; i < spritesheet_height[size]; i ++)
        ts[index].data[i] = ts[index].storage + i * spritesheet_width[size];
}

void spritesheet_set_data_from_file (spritesheet_index_t id, const char *filename)
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

        ts_width = spritesheet_width[ts[id].size];
        ts_height = spritesheet_height[ts[id].size];

        width = MIN(img_width, ts_width);
        height = MIN(img_height, ts_height);
        
        for (unsigned j = 0; j < height; j ++)
        {
            for (unsigned i = 0; i < width ; i ++)
            {
                ts[id].data[j][i] = img_store[j * img_stride + i];
            }
        }
        if (id == SPRITESHEET_MAIN)
            g_debug ("loaded pixbuf %s as bg main spritesheet", path);
        else
            g_debug ("loaded pixbuf %s as bg sub spritesheet", path);
        g_free (path);
        g_object_unref (pb);
    }
}

SCM_DEFINE (G_spritesheet_init, "spritesheet-init", 3, 0, 0,
            (SCM id, SCM size, SCM bank), "\
Set the size and VRAM storage of a given spritesheet")
{
    spritesheet_init (scm_to_int (id), scm_to_int (size), scm_to_int (bank));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_spritesheet_set_data_from_file, "spritesheet-set-data-from-file",
            2, 0, 0, (SCM id, SCM filename), "\
Copies the contents of an ARGB32 image into the spritesheet.  Note that the \n\
spritesheet's size and VRAM must first be set using 'spritesheet-init'")
{
    char *str = scm_to_locale_string (filename);
    spritesheet_set_data_from_file (scm_to_int (id), str);
    free (str);
    return SCM_UNSPECIFIED;
}

SCM_VARIABLE_INIT (G_SPRITESHEET_MAIN, "SPRITESHEET_MAIN",
                   scm_from_int (SPRITESHEET_MAIN));
SCM_VARIABLE_INIT (G_SPRITESHEET_SUB, "SPRITESHEET_SUB",
                   scm_from_int (SPRITESHEET_SUB));

SCM_VARIABLE_INIT (G_SPRITESHEET_SIZE_32x32, "SPRITESHEET_SIZE_32x32", scm_from_int (SPRITESHEET_SIZE_32x32));
SCM_VARIABLE_INIT (G_SPRITESHEET_SIZE_128x128, "SPRITESHEET_SIZE_128x128", scm_from_int (SPRITESHEET_SIZE_128x128));
SCM_VARIABLE_INIT (G_SPRITESHEET_SIZE_256x256, "SPRITESHEET_SIZE_256x256", scm_from_int (SPRITESHEET_SIZE_256x256));
SCM_VARIABLE_INIT (G_SPRITESHEET_SIZE_256x512, "SPRITESHEET_SIZE_256x512", scm_from_int (SPRITESHEET_SIZE_256x512));
SCM_VARIABLE_INIT (G_SPRITESHEET_SIZE_512x256, "SPRITESHEET_SIZE_512x256", scm_from_int (SPRITESHEET_SIZE_512x256));
SCM_VARIABLE_INIT (G_SPRITESHEET_SIZE_512x512, "SPRITESHEET_SIZE_512x512", scm_from_int (SPRITESHEET_SIZE_512x512));

void
spritesheet_init_guile_procedures (void)
{
#include "spritesheet.x"
    scm_c_export ("spritesheet-init",
                  "spritesheet-set-data-from-file",
                  "SPRITESHEET_SIZE_32x32", 
                  "SPRITESHEET_SIZE_128x128", 
                  "SPRITESHEET_SIZE_256x256", 
                  "SPRITESHEET_SIZE_256x512", 
                  "SPRITESHEET_SIZE_512x256", 
                  "SPRITESHEET_SIZE_512x512", 
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
