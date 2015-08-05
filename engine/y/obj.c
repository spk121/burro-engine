#include <stdint.h>
#include <stdbool.h>
#include "../x.h"
#include "eng.h"
#include "guile.h"
#include "obj.h"
#include "spritesheet.h"

struct obj_matrix
{
    obj_size_t size;
    uint32_t *storage;
    uint32_t **data;
};

size_t obj_matrix_width[12] = {
    [OBJ_SIZE_16x16] = 16,
    [OBJ_SIZE_16x32] = 16,
    [OBJ_SIZE_16x64] = 16,
    [OBJ_SIZE_32x16] = 32,
    [OBJ_SIZE_32x32] = 32,
    [OBJ_SIZE_32x64] = 32,
    [OBJ_SIZE_64x16] = 16,
    [OBJ_SIZE_64x32] = 64,
    [OBJ_SIZE_64x64] = 64,
    [OBJ_SIZE_64x128] = 64,
    [OBJ_SIZE_128x64] = 128,
    [OBJ_SIZE_128x128] = 128,
};

size_t obj_matrix_height[12] = {
    [OBJ_SIZE_16x16] = 16,
    [OBJ_SIZE_16x32] = 32,
    [OBJ_SIZE_16x64] = 64,
    [OBJ_SIZE_32x16] = 16,
    [OBJ_SIZE_32x32] = 32,
    [OBJ_SIZE_32x64] = 64,
    [OBJ_SIZE_64x16] = 16,
    [OBJ_SIZE_64x32] = 32,
    [OBJ_SIZE_64x64] = 64,
    [OBJ_SIZE_64x128] = 128,
    [OBJ_SIZE_128x64] = 64,
    [OBJ_SIZE_128x128] = 128,
};

size_t obj_matrix_size[12] = {
    [OBJ_SIZE_16x16] = 16*16,
    [OBJ_SIZE_16x32] = 16*32,
    [OBJ_SIZE_16x64] = 16*64,
    [OBJ_SIZE_32x16] = 32*16,
    [OBJ_SIZE_32x32] = 32*32,
    [OBJ_SIZE_32x64] = 32*64,
    [OBJ_SIZE_64x16] = 64*16,
    [OBJ_SIZE_64x32] = 64*32,
    [OBJ_SIZE_64x64] = 64*64,
    [OBJ_SIZE_64x128] = 64*128,
    [OBJ_SIZE_128x64] = 128*64,
    [OBJ_SIZE_128x128] = 128*128,
};

static size_t
obj_matrix_get_height (obj_size_t size)
{
    return obj_matrix_height[size];
}

static size_t
obj_matrix_get_width (obj_size_t size)
{
    return obj_matrix_width[size];
}

static size_t
obj_matrix_get_u32_size (obj_size_t size)
{
    return obj_matrix_size[size];
} 

typedef struct obj_entry
{
    /** Sprite is visible if true */
    bool enable;

    /** priority aka z-level. 0 to 3 where 0 is foreground */
    int priority;

    /** location of top-left corner of sprite in sprite sheet */
    int spritesheet_i, spritesheet_j;

    /** size of sprite in pixels */
    obj_size_t size;

    /** the "user" or screen location of the rotation center of the sprite */
    double x, y;

    /** the "device" location of the rotation center, aka, it bitmap row and column of its hotspot*/
    double rotation_center_x, rotation_center_y;

    /** the expansion factor of the sprite: 1.0 = 1 pixel per pixel */
    double expansion;

    /** the rotation angle of the sprite about its rotation center, in radians */
    double rotation;

    /** Flip object vertically or horizontally */
    bool hflip;
    bool vflip;

    /* Invert color of object */
    bool colorswap;

    /** Adjust color of object: from  0.0 to 1.0 */
    double brightness;

    cairo_surface_t *surf;
} obj_entry_t;

typedef struct obj_tag
{
    obj_entry_t obj[MAIN_OBJ_COUNT + SUB_OBJ_COUNT];
} obj_t;

obj_t obj;

////////////////////////////////////////////////////////////////

static uint32_t
adjust_colorval (uint32_t c32, bool colorswap, double brightness)
{
    uint32_t a, r, g, b;
    a = (((uint32_t) c32 & 0xFF000000) >> 24);
    r = (((uint32_t) c32 & 0x00FF0000) >> 16);
    g = (((uint32_t) c32 & 0x0000FF00) >> 8);
    b = ((uint32_t) c32 & 0x000000FF);
    if (colorswap)
    {
        uint32_t temp = r;
        r = b;
        b = temp;
    } 
    r = r * brightness;
    g = g * brightness;
    b = b * brightness;
    return (a << 24) + (r << 16) + (g << 8) + b;
}

////////////////////////////////////////////////////////////////


void obj_hide (int id)
{
    obj.obj[id].enable = false;
}

void obj_show (int id)
{
    obj.obj[id].enable = true;
}

bool obj_is_shown (int id)
{
    return obj.obj[id].enable;
}

void obj_init (int id, int spritesheet_i, int spritesheet_j, obj_size_t size,
               double rotation_center_x, double rotation_center_y, bool hflip, bool vflip)
{
    obj.obj[id].spritesheet_i = spritesheet_i;
    obj.obj[id].spritesheet_j = spritesheet_j;
    obj.obj[id].size = size;
    obj.obj[id].rotation_center_x = rotation_center_x;
    obj.obj[id].rotation_center_y = rotation_center_y;
    obj.obj[id].hflip = hflip;
    obj.obj[id].vflip = vflip;
}

void obj_set_spritesheet_origin (int id, int spritesheet_i, int spritesheet_j)
{
    obj.obj[id].spritesheet_i = spritesheet_i;
    obj.obj[id].spritesheet_j = spritesheet_j;
}

void obj_set (int id, int priority, double x, double y, double rotation, double expansion)
{
    obj.obj[id].priority = priority;
    obj.obj[id].x = x;
    obj.obj[id].y = y;
    obj.obj[id].rotation = rotation;
    obj.obj[id].expansion = expansion;
}

int obj_get_priority (int id)
{
    return obj.obj[id].priority;
}

void obj_set_rotation_expansion (int id, double rotation, double expansion)
{
    obj.obj[id].rotation = rotation;
    obj.obj[id].expansion = expansion;
}

void obj_set_position (int id, double x, double y)
{
    obj.obj[id].x = x;
    obj.obj[id].y = y;
}

void obj_get_location (int id, double *x, double *y, double *rotation_center_x, double *rotation_center_y,
                       double *rotation, double *expansion)
{
    *x = obj.obj[id].x;
    *y = obj.obj[id].y;
    *rotation_center_x = obj.obj[id].rotation_center_x;
    *rotation_center_y = obj.obj[id].rotation_center_y;
    *rotation = obj.obj[id].rotation;
    *expansion = obj.obj[id].expansion;
}

void obj_set_spritesheet_from_file (int spritesheet_id, const char *filename)
{
    g_return_if_fail (spritesheet_id < 0 || spritesheet_id >= SPRITESHEET_COUNT);
    g_return_if_fail (filename == NULL);
    char *path = xg_find_data_file (filename);
    g_return_if_fail (path != NULL);
    GdkPixbuf *pb = xgdk_pixbuf_new_from_file (filename);
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

        width = CLAMP(width, 0, spritesheet_get_width(spritesheet_id));
        height = CLAMP(height, 0, spritesheet_get_height(spritesheet_id));
        for (int j = 0; j < height; j ++)
        {
            memcpy (spritesheet_get_u32_data(spritesheet_id)[j], c32 + j * stride, width * sizeof(uint32_t));
        }
        if (spritesheet_id == 0)
            g_debug ("loaded pixbuf %s as main spritesheet", path);
        else
            g_debug ("loaded pixbuf %s as sub spritesheet", path);
        g_free (path);
        g_object_unref (pb);
    }
}

#if 0
void obj_set_tilesheet_from_tga (int sub_flag, targa_image_t *t)
{
    guint width, height;
    obj_data_t *osheet;
    int first = targa_get_color_map_first_index (t);
    if (sub_flag)
        osheet = &(objsheet[1]);
    else
        osheet = &(objsheet[0]);

    targa_get_image_dimensions (t, &width, &height);
    for (guint j = 0; j < height; j ++)
    {
        for (guint i = 0; i < width; i ++)
        {
            osheet->bmp[j][i] = tga_get_image_data_u8_ptr(t)[j * width + i];
        }
    }

    for (guint i = 0; i < targa_get_color_map_length (t) - first; i ++)
        osheet->palette[i] = tga_get_color_map_data_u16_ptr(t)[i + first];

}
#endif

cairo_surface_t *obj_render_to_cairo_surface (int id)
{
    guint width, height, stride;
    uint32_t *data, c32;
    cairo_surface_t *surf;
    int spritesheet_width, spritesheet_height, spritesheet_stride;
    int spritesheet_id;
    
    g_return_val_if_fail (id < 0 || id >= MAIN_OBJ_COUNT + SUB_OBJ_COUNT, NULL);

    width = obj_matrix_get_width (obj.obj[id].size);
    height = obj_matrix_get_height (obj.obj[id].size);

    if (id < MAIN_OBJ_COUNT)
        spritesheet_id = 0;
    else if (id >= MAIN_OBJ_COUNT && id < MAIN_OBJ_COUNT + SUB_OBJ_COUNT)
        spritesheet_id = 1;
    else
        abort ();

    surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
    data = xcairo_image_surface_get_argb32_data (surf);
    stride = xcairo_image_surface_get_argb32_stride (surf);
    xcairo_surface_flush (surf);

    if ((obj.obj[id].vflip == FALSE)
        && (obj.obj[id].hflip == FALSE)
        && (obj.obj[id].brightness == 1.0)
        && (obj.obj[id].colorswap == false))
    {
        // FAST PATH
        g_assert (stride == width);
        for (unsigned j = 0; j < height; j ++)
        {
                memcpy (data + j * stride,
                        &(spritesheet_get_u32_data(spritesheet_id)[obj.obj[id].spritesheet_j + j][obj.obj[id].spritesheet_i]),
                        width * sizeof (uint32_t));
        }
    }
    else
    {
        // SLOW PATH
        for (guint j = 0; j < height; j++)
        {
            for (guint i = 0; i < width; i++)
            {
                guint si, sj;
                sj = j + obj.obj[id].spritesheet_j;
                if (obj.obj[id].vflip == TRUE)
                    sj = height - sj;
                si = i + obj.obj[id].spritesheet_i;
                if (obj.obj[id].hflip == TRUE)
                    si = width - si;

                c32 = spritesheet_get_u32_data(spritesheet_id)[sj][si];
                    
                data[j * stride + i] = adjust_colorval (c32, obj.obj[id].colorswap, obj.obj[id].brightness);
            }
        }
    }
    xcairo_surface_mark_dirty (surf);
    return surf;
}

static void
obj_update (int id)
{
    if (obj.obj[id].surf != NULL)
        xcairo_surface_destroy (obj.obj[id].surf);
    obj.obj[id].surf = obj_render_to_cairo_surface (id);
}


////////////////////////////////////////////////////////////////

SCM_DEFINE (G_obj_hide, "obj-hide", 1, 0, 0, (SCM gid), "\
Set object to not draw.")
{
    obj_hide (scm_to_int (gid));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_obj_show, "obj-show", 1, 0, 0, (SCM gid), "\
Set object to draw.")
{
    unsigned id = scm_to_int (gid);
    // unsigned id = guile_to_ranged_uint_or_error ("obj-hide", SCM_ARG1, OBJ_COUNT, gid);
    obj_hide (id);
    return SCM_UNSPECIFIED;   
}

SCM_DEFINE (G_obj_shown_p, "obj-shown?", 1, 0, 0, (SCM gid), "")
{
    return scm_from_bool (obj_is_shown (scm_to_int (gid)));
}

SCM_DEFINE (G_obj_init, "obj-init", 9, 0, 0,
            (SCM id, SCM spritesheet_i, SCM spritesheet_j, SCM sprite_size,
             SCM rot_center_x, SCM rot_center_y, SCM hflip, SCM vflip), "")
{
    obj_init (scm_to_int (id),
              scm_to_int (spritesheet_i),
              scm_to_int (spritesheet_j),
              scm_to_int (sprite_size),
              scm_to_double (rot_center_x),
              scm_to_double (rot_center_y),
              scm_to_bool (hflip),
              scm_to_bool (vflip));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_obj_set, "obj-set", 6, 0, 0, (SCM id, SCM priority, SCM x, SCM y, SCM rot, SCM exp),"")
{
    obj_set (scm_to_int (id),
             scm_to_int (priority),
             scm_to_double (x),
             scm_to_double (y),
             scm_to_double (rot),
             scm_to_double (exp));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_obj_set_rotation_expansion, "obj-set-rotation-expansion", 3, 0, 0, (SCM id, SCM rot, SCM exp), "")
{
    obj_set_rotation_expansion (scm_to_int (id),
                                scm_to_double (rot),
                                scm_to_double (exp));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_obj_set_position, "obj-set-position", 3, 0, 0, (SCM id, SCM x, SCM y), "")
{
    obj_set_position (scm_to_int (id),
                      scm_to_double (x),
                      scm_to_double (y));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_obj_set_spritesheet_origin, "obj-set-spritesheet-origin", 3, 0, 0, (SCM id, SCM i, SCM j), "")
{
    obj_set_spritesheet_origin (scm_to_int (id), scm_to_int (i), scm_to_int (j));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_obj_set_tilesheet_from_file, "obj-set-spritesheet-from-file", 2, 0, 0, (SCM sub, SCM filename),"")
{
    char *fname = scm_to_locale_string (filename);
    obj_set_spritesheet_from_file (scm_to_int (sub), fname);
    free (fname);
    return SCM_UNSPECIFIED;
}

void
init_guile_obj_procedures (void)
{
#include "obj.x"
    scm_c_export (
        "obj-init",

        "obj-hide",
        "obj-show",

        "obj-set",
        "obj-rotate",
        "obj-move",
        "obj-set-position"
        "obj-set-expansion",
        "obj-set-rotation",
        "obj-set-rotation-center",
        "obj-set-rotation-expansion",

        "obj-set-colorswap",
        "obj-set-brightness",

        "obj-get-width",
        "obj-get-height",
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
