#include <stdint.h>
#include <stdbool.h>
#include "../x.h"
#include "bg.h"
#include "eng.h"
#include "tga.h"
#include "vram.h"

struct bg_matrix
{
    int width, height;
    uint32_t *storage;
    uint32_t **data;
};

size_t bg_matrix_width[9] = {
    [BG_SIZE_16x16] = 16,
    [BG_SIZE_32x16] = 32,
    [BG_SIZE_16x32] = 16,
    [BG_SIZE_32x32] = 32,
    [BG_SIZE_128x128] = 128,
    [BG_SIZE_256x256] = 256,
    [BG_SIZE_512x256] = 512,
    [BG_SIZE_256x512] = 256,
    [BG_SIZE_512x512] = 512,
};

size_t bg_matrix_height[9] = {
    [BG_SIZE_16x16] = 16,
    [BG_SIZE_32x16] = 16,
    [BG_SIZE_16x32] = 32,
    [BG_SIZE_32x32] = 32,
    [BG_SIZE_128x128] = 128,
    [BG_SIZE_256x256] = 256,
    [BG_SIZE_512x256] = 256,
    [BG_SIZE_256x512] = 512,
    [BG_SIZE_512x512] = 512,
};

size_t bg_matrix_size[9] = {
    [BG_SIZE_16x16] = 16*16,
    [BG_SIZE_32x16] = 32*16,
    [BG_SIZE_16x32] = 16*32,
    [BG_SIZE_32x32] = 32*32,
    [BG_SIZE_128x128] = 128*128,
    [BG_SIZE_256x256] = 256*256,
    [BG_SIZE_512x256] = 512*256,
    [BG_SIZE_256x512] = 256*512,
    [BG_SIZE_512x512] = 512*512,
};

static void
_bg_allocate_matrix (struct bg_matrix *x, bg_size_t size, vram_bank_t bank)
{
    g_assert (x != NULL);
    
    g_free (x->data);

    g_assert_cmpuint (bg_matrix_get_u32_size(size), >=,  vram_get_u32_size(bank));
    
    vram_zero_bank(bank);
    x->storage = vram_get_ptr(bank);
    x->data = g_new0(uint32_t *, bg_matrix_get_height(size));
    for (size_t i = 0; i < bg_matrix_height[size]; i ++)
        x->data[i] = x->storage + i * bg_matrix_width[size];
}

typedef struct bg_entry
{
    /* BG is displayed when true */
    bool enable;

    /** tile and map, palette bmp, or true color bmp */
    bg_type_t type;

    /* z-level: 0 is foreground, 3 is background */
    int priority;

    /** the "user" or screen location of the rotation center of the
     * background */
    double scroll_x, scroll_y;

    /** the "device" location of the rotation center of the background*/
    double rotation_center_x, rotation_center_y;

    /** the expansion factor of the background: 1.0 = 1 pixel per pixel */
    double expansion;

    /** the rotation angle of the background about its rotation
     * center, in radians */
    double rotation;

    /** The width, height, and data of either the map or the bitmap.
     *  If this is a map, data contains indices.  If this is a bitmap,
     *  data contains colorrefs.
     *  FIXME: replace this with VRAM to save space */
    int width;
    int height;
    uint32_t data[BG_DATA_WIDTH][BG_DATA_HEIGHT];
} bg_entry_t;

typedef struct bg_tag
{
    /* The RGBA color displayed below all backgrounds and sprites */
    uint32_t bg_color;

    bool colorswap;
  
    double brightness;

    uint32_t main_tilesheet[BG_TILESHEET_HEIGHT][BG_TILESHEET_WIDTH];
    uint32_t sub_tilesheet[BG_TILESHEET_HEIGHT][BG_TILESHEET_WIDTH];
    
    bg_entry_t bg[BG_MAIN_BACKGROUNDS_COUNT + BG_SUB_BACKGROUNDS_COUNT];
} bg_t;

bg_t bg;

static cairo_surface_t *
bg_render_map_to_cairo_surface (int id);
static cairo_surface_t *
bg_render_bmp_to_cairo_surface (int id);

static uint32_t
adjust_colorval (uint32_t c32)
{
    uint32_t a, r, g, b;

    a = (((uint32_t) c32 & 0xff000000) >> 24);
    r = (((uint32_t) c32 & 0x00ff0000) >> 16);
    g = (((uint32_t) c32 & 0x0000ff00) >> 8);
    b = ((uint32_t) c32 & 0x000000ff);

    if (bg.colorswap)
    {
        uint32_t temp = r;
        r = b;
        b = temp;
    }
    r = r * bg.brightness;
    g = g * bg.brightness;
    b = b * bg.brightness;
    c32 = (a << 24) + (r << 16) + (g << 8) + b;
    return c32;
}

void bg_get_backdrop_color_rgb (double *r, double *g, double *b)
{
  uint32_t c32 = adjust_colorval (bg.bg_color);
    *r = ((double)((c32 & 0x00ff0000) >> 16)) / 255.0;
    *g = ((double)((c32 & 0x0000ff00) >> 8)) / 255.0;
    *b = ((double)((c32 & 0x000000ff))) / 255.0;
}

bool
bg_is_shown (int id)
{
    return bg.bg[id].enable;
}

uint32_t *bg_get_data_ptr (int id)
{
    return &(bg.bg[id].data[0][0]);
}

uint32_t *bg_get_tilesheet_ptr (int id)
{
    if (id >= BG_SUB_0 && id <= BG_SUB_3)
        return &(bg.sub_tilesheet[0][0]);
    return &(bg.main_tilesheet[0][0]);
    
}

int bg_get_priority (int id)
{
    return bg.bg[id].priority;
}

void bg_hide (int id)
{
    bg.bg[id].enable = FALSE;
}

void bg_init()
{
    bg.bg_color = 0xff000000;
    bg.colorswap = false;
    bg.brightness = 1.0;
    for (int i = 0; i < BG_MAIN_BACKGROUNDS_COUNT + BG_SUB_BACKGROUNDS_COUNT; i ++)
    {
        bg.bg[i].enable = false;
        bg.bg[i].type = BG_TYPE_BMP;
        bg.bg[i].priority = i % 4;
        bg.bg[i].scroll_x = 0.0;
        bg.bg[i].scroll_y = 0.0;
        bg.bg[i].rotation_center_x = 0.0;
        bg.bg[i].rotation_center_y = 0.0;
        bg.bg[i].expansion = 1.0;
        bg.bg[i].rotation = 0.0;
        bg.bg[i].height = 0;
        bg.bg[i].width = 0;
    }
}

void bg_reset (int id, bg_type_t type)
{
    bg.bg[id].type = type;
    bg.bg[id].scroll_x = 0.0;
    bg.bg[id].scroll_y = 0.0;
    bg.bg[id].rotation_center_x = 0.0;
    bg.bg[id].rotation_center_y = 0.0;
    bg.bg[id].expansion = 1.0;
    bg.bg[id].rotation = 0.0;
    bg.bg[id].priority = id % 4;
    bg.bg[id].height = 0;
    bg.bg[id].width = 0;
}

void bg_rotate (int id, double angle)
{
    bg.bg[id].rotation += angle;
}

void bg_scroll (int id, double dx, double dy)
{
    bg.bg[id].scroll_x += dx;
    bg.bg[id].scroll_y += dy;
}

void bg_set (int id, double rotation, double expansion,
             double scroll_x, double scroll_y,
             double rotation_center_x, double rotation_center_y)
{
    bg.bg[id].rotation = rotation;
    bg.bg[id].expansion = expansion;
    bg.bg[id].scroll_x = scroll_x;
    bg.bg[id].scroll_y = scroll_y;
    bg.bg[id].rotation_center_x = rotation_center_x;
    bg.bg[id].rotation_center_y = rotation_center_y;
}

void bg_set_backdrop_color (uint32_t c32)
{
    bg.bg_color = c32;
}

void bg_set_expansion (int id, double expansion)
{
    bg.bg[id].expansion = expansion;
}

void bg_set_priority (int id, int priority)
{
    bg.bg[id].priority = priority;
}

void bg_set_rotation (int id, double rotation)
{
    bg.bg[id].rotation = rotation;
}

void bg_set_rotation_center (int id,
                             double rotation_center_x, double rotation_center_y)
{
    bg.bg[id].rotation_center_x = rotation_center_x;
    bg.bg[id].rotation_center_y = rotation_center_y;
}

void bg_set_rotation_expansion (int id, double rotation, double expansion)
{
    bg.bg[id].rotation = rotation;
    bg.bg[id].expansion = expansion;
}

void bg_show (int id)
{
    bg.bg[id].enable = TRUE;
}

#if 0
static void bg_set_map_from_tga (int id, targa_image_t *t)
{
    unsigned width, height;
    targa_get_image_dimensions (t, &width, &height);
    bg.bg[id].map.width = width;
    bg.bg[id].map.height = height;
    for (unsigned j = 0; j < height; j ++)
    {
        for (unsigned i = 0; i < width ; i ++)
        {
            bg.bg[id].map.map[j][i] = tga_get_image_data_u16_ptr(t)[j * width + i];
        }
    }
}
#endif

#if 0
static void bg_set_tilesheet_from_tga (int id, targa_image_t *t)
{
    unsigned width, height;
    int first = targa_get_color_map_first_index (t);

    targa_get_image_dimensions (t, &width, &height);

    for (unsigned j = 0; j < height; j ++)
    {
        for (unsigned i = 0; i < width ; i ++)
        {
            bg.bg[id].map.tilesheet[j][i] = tga_get_image_data_u8_ptr(t)[j * width + i];
        }
    }

    for (unsigned i = 0; i < targa_get_color_map_length (t) - first; i ++)
        bg.bg[id].map.palette[i] = tga_get_color_map_data_u16_ptr(t)[i + first];
}
#endif

static void set_from_image_file (int id, bg_type_t type, const char *filename)
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
        int width, height, stride;
        xgdk_pixbuf_get_width_height_stride (pb, &width, &height, &stride);
        uint32_t *c32 = xgdk_pixbuf_get_argb32_pixels (pb);

        if (id == -2 || id == -1)
        {
            if (width > BG_TILESHEET_WIDTH)
                width = BG_TILESHEET_WIDTH;
            if (height > BG_TILESHEET_HEIGHT)
                height = BG_TILESHEET_HEIGHT;
        }
        else
        {
            if (width > BG_DATA_WIDTH)
                width = BG_DATA_WIDTH;
            if (height > BG_DATA_HEIGHT)
                height = BG_DATA_HEIGHT;
            bg.bg[id].height = height;
            bg.bg[id].width = width;
            bg.bg[id].type = type;
        }
        
        for (unsigned j = 0; j < height; j ++)
        {
            for (unsigned i = 0; i < width ; i ++)
            {
                if (id == -2)
                    bg.sub_tilesheet[j][i] = c32[j * stride + i];
                else if (id == -1)
                    bg.main_tilesheet[j][i] = c32[j * stride + i];
                else
                    bg.bg[id].data[j][i] = c32[j * stride + i];
            }
        }
        if (id == -2)
            g_debug ("loaded pixbuf %s as bg sub tilesheet", path);
        else if (id == -1)
            g_debug ("loaded pixbuf %s as bg main tilesheet", path);
        else
            g_debug ("loaded pixbuf %s as bg %d", path, id);
        g_free (path);
        g_object_unref (pb);
    }
}


void bg_set_data_from_image_file (int id, bg_type_t type, const char *filename)
{
    set_from_image_file (id, type, filename);
}

void bg_set_tilesheet_from_image_file (int id, const char *filename)
{
    if (id == 0)
        set_from_image_file (-1, 0, filename);
    else if (id == 1)
        set_from_image_file (-2, 0, filename);
}

#if 0
void bg_set_bmp_from_resource (int id, const gchar *resource)
{
    targa_image_t *t = tga_load_from_resource (resource);
    bg_set_bmp_from_tga (id, t);
    tga_free (t);
}
#endif

cairo_surface_t *
bg_render_to_cairo_surface (int id)
{
    g_return_if_fail (id >= 0 && id < BG_MAIN_BACKGROUNDS_COUNT + BG_SUB_BACKGROUNDS_COUNT);
    
    switch (bg.bg[id].type)
    {
    case BG_TYPE_MAP:
        return bg_render_map_to_cairo_surface (id);
        break;
    case BG_TYPE_BMP:
        return bg_render_bmp_to_cairo_surface (id);
        break;
    }
    g_return_val_if_reached (NULL);
}

static cairo_surface_t *
bg_render_map_to_cairo_surface (int id)
{
    cairo_surface_t *surf;
    uint32_t *data;
    int stride;
    int tile_j, tile_i, delta_tile_i, delta_tile_j;
    int map_index;
    uint32_t c;
    int width, height;

    width = bg.bg[id].width;
    height = bg.bg[id].height;

    g_return_val_if_fail (width > 0 && height > 0, NULL);
    
    surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                        width * BG_TILE_WIDTH,
                                        height * BG_TILE_HEIGHT);
    data = xcairo_image_surface_get_argb32_data (surf);
    stride = xcairo_image_surface_get_argb32_stride (surf);
    xcairo_surface_flush (surf);

    for (unsigned map_j = 0; map_j < height; map_j ++)
    {
        for (unsigned map_i = 0; map_i < width; map_i ++)
        {
            /* Fill in the tile brush */
            map_index = bg.bg[id].data[map_j][map_i];
            delta_tile_j = (map_index / BG_TILESHEET_WIDTH_IN_TILES) * BG_TILE_HEIGHT;
            delta_tile_i = (map_index % BG_TILESHEET_WIDTH_IN_TILES) * BG_TILE_WIDTH;
            for (tile_j = 0; tile_j < BG_TILE_HEIGHT; tile_j ++)
            {
                for (tile_i = 0; tile_i < BG_TILE_WIDTH; tile_i ++)
                {
                    uint32_t c32;
                    if (id >= BG_MAIN_0 && id <= BG_MAIN_3)
                        c32 = bg.main_tilesheet[delta_tile_j + tile_j][delta_tile_i + tile_i];
                    else if (id >= BG_SUB_0 && id <= BG_SUB_3)
                        c32 = bg.sub_tilesheet[delta_tile_j + tile_j][delta_tile_i + tile_i];
                    else
                        g_return_val_if_reached (NULL);

                    c = adjust_colorval (c32);
                    data[(map_j * BG_TILE_HEIGHT + tile_j) * stride
                         + (map_i * BG_TILE_WIDTH + tile_i)] = c;
                }
            }
        }
    }
    xcairo_surface_mark_dirty (surf);
    return surf;
}

static cairo_surface_t *
bg_render_bmp_to_cairo_surface (int id)
{
    int width, height, stride;
    uint32_t *data;
    uint32_t c32;
    cairo_surface_t *surf;

    width = bg.bg[id].width;
    height = bg.bg[id].height;

    g_return_val_if_fail (width > 0 && height > 0, NULL);

    surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
    data = xcairo_image_surface_get_argb32_data (surf);
    stride = xcairo_image_surface_get_argb32_stride (surf);
    xcairo_surface_flush (surf);
    for (unsigned j = 0; j < height; j++)
    {
        for (unsigned i = 0; i < width; i++)
        {
            c32 = bg.bg[id].data[j][i];
            data[j * stride + i] = adjust_colorval (c32);
        }
    }
    xcairo_surface_mark_dirty (surf);
    return surf;
}

void bg_get_transform (int id, double *scroll_x, double *scroll_y, double *rotation_center_x,
                       double *rotation_center_y, double *rotation, double *expansion)
{
    *scroll_x = bg.bg[id].scroll_x;
    *scroll_y = bg.bg[id].scroll_y;
    *rotation_center_x = bg.bg[id].rotation_center_x;
    *rotation_center_y = bg.bg[id].rotation_center_y;
    *rotation = bg.bg[id].rotation;
    *expansion = bg.bg[id].expansion;
}

SCM_DEFINE (G_bg_get_priority, "bg-get-priority", 1, 0, 0, (SCM id), "\
return the z-ordering of a given background layer.")
{
    return scm_from_int (bg_get_priority (scm_to_int (id)));
}

SCM_DEFINE (G_bg_hide, "bg-hide", 1, 0, 0, (SCM id), "\
Set background later ID to not be drawn")
{
    bg_hide (scm_to_int (id));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_reset, "bg-reset", 2, 0, 0,
            (SCM id, SCM type), "\
Reset a background to hidden with nominal status")
{
    bg_reset (scm_to_int (id),
             (bg_type_t) scm_to_int (type));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_rotate, "bg-rotate", 2, 0, 0, (SCM id, SCM angle), "\
Rotate the background about its rotation center")
{
    bg_rotate (scm_to_int (id), scm_to_double (angle));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_scroll, "bg-scroll", 3, 0, 0, (SCM id, SCM dx, SCM dy), "\
Move the background")
{
    bg_scroll (scm_to_int (id), scm_to_double (dx), scm_to_double (dy));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_set, "bg-set", 7, 0, 0,
            (SCM id, SCM rotation, SCM expansion, SCM scroll_x, SCM scroll_y, SCM center_x, SCM center_y), "\
Set the position, rotation, expansion, and rotation center of a background.")
{
    bg_set (scm_to_int(id), scm_to_double(rotation), scm_to_double (expansion), scm_to_double (scroll_x),
            scm_to_double (scroll_y), scm_to_double (center_x), scm_to_double (center_y));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_set_backdrop_color, "bg-set-backdrop-color",
            1, 0, 0, (SCM color), "\
Given COLOR, a 32-bit ARGB integer, set the color of the lowest layer of\n\
the background")
{
  bg_set_backdrop_color (scm_to_uint32 (color));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_set_bmp_from_file, "bg-set-bmp-from-file",
            2, 0, 0, (SCM id, SCM filename), "\
Set BG to be a bitmap-type background using the data from FILE in the data\n\
directory")
{
    char *str = scm_to_locale_string (filename);
    bg_set_bmp_from_file (scm_to_int (id), str);
    free (str);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_set_expansion, "bg-set-expansion", 2, 0, 0, (SCM id, SCM expansion), "\
Set BG expansion")
{
    bg_set_expansion (scm_to_int (id), scm_to_double (expansion));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_set_priority, "bg-set-priority", 2, 0, 0, (SCM id, SCM priority), "\
Set BG priority")
{
    bg_set_priority (scm_to_int (id), scm_to_int (priority));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_set_rotation, "bg-set-rotation", 2, 0, 0, (SCM id, SCM rotation), "\
Set BG rotation")
{
    bg_set_rotation (scm_to_int (id), scm_to_int (rotation));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_set_rotation_center, "bg-set-rotation-center", 3, 0, 0, (SCM id, SCM x, SCM y), "\
Move the rotation center of the background")
{
    bg_set_rotation_center (scm_to_int (id), scm_to_double (x), scm_to_double (y));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_set_rotation_expansion, "bg-set-rotation-expansion", 3, 0, 0, (SCM id, SCM r, SCM e), "\
Set the rotation angle and expansion of a BG")
{
    bg_set_rotation_expansion (scm_to_int (id), scm_to_double (r), scm_to_double (e));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_show, "bg-show", 1, 0, 0, (SCM id), "\
Set background later ID to be drawn")
{
    bg_show (scm_to_int (id));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_bg_shown_p, "bg-shown?", 1, 0, 0, (SCM id), "\
Return #t if indicated background layer is visible.")
{
    return scm_from_bool (bg_is_shown (scm_to_int (id)));
}

void
bg_init_guile_procedures (void)
{
#include "bg.x"
    scm_c_export ("bg-get-priority",
                  "bg-hide",
                  "bg-reset",
                  "bg-rotate",
                  "bg-scroll",
                  "bg-set",
                  "bg-set-backdrop-color",
                  "bg-set-bmp-from-file",
                  "bg-set-expansion",
                  "bg-set-priority",
                  "bg-set-rotation",
                  "bg-set-rotation-center",
                  "bg-set-rotation-expansion",
                  "bg-show",
                  "bg-shown?",
                  NULL);
}

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
