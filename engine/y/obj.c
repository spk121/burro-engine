#include "../x/xcairo.h"
#include "../x/xglib.h"
#include "../x/xgdk-pixbuf.h"
#include "eng.h"
#include "guile.h"
#include "obj.h"
#include <stdint.h>
#include <stdbool.h>

typedef struct obj_entry
{
    /** Sprite is visible if true */
    bool enable;

    /** priority aka z-level. 0 to 3 where 0 is foreground */
    int priority;

    /** location of top-left corner of sprite in sprite sheet */
    int spritesheet_i, spritesheet_j;

    /** size of sprite in pixels */
    int sprite_width, sprite_height;

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
} obj_entry_t;

typedef struct obj_data
{
    uint32_t bmp[OBJSHEET_HEIGHT][OBJSHEET_WIDTH];
} obj_data_t;

obj_entry_t obj[MAIN_OBJ_COUNT + SUB_OBJ_COUNT];

static bool colorswap = false;
static double brightness = 1.0;

static GdkPixbuf *main_pixbuf = NULL;
static GdkPixbuf *sub_pixbuf = NULL;
    

/****************************************************************/

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

void obj_hide (int id)
{
    obj[id].enable = false;
}

void obj_show (int id)
{
    obj[id].enable = true;
}

bool obj_is_shown (int id)
{
    return obj[id].enable;
}

void obj_init (int id, int spritesheet_i, int spritesheet_j, int sprite_width, int sprite_height,
               double rotation_center_x, double rotation_center_y, bool hflip, bool vflip)
{
    obj[id].spritesheet_i = spritesheet_i;
    obj[id].spritesheet_j = spritesheet_j;
    obj[id].sprite_width = sprite_width;
    obj[id].sprite_height = sprite_height;
    obj[id].rotation_center_x = rotation_center_x;
    obj[id].rotation_center_y = rotation_center_y;
    obj[id].hflip = hflip;
    obj[id].vflip = vflip;
}

void obj_set_spritesheet_origin (int id, int spritesheet_i, int spritesheet_j)
{
    obj[id].spritesheet_i = spritesheet_i;
    obj[id].spritesheet_j = spritesheet_j;
}

void obj_set (int id, int priority, double x, double y, double rotation, double expansion)
{
    obj[id].priority = priority;
    obj[id].x = x;
    obj[id].y = y;
    obj[id].rotation = rotation;
    obj[id].expansion = expansion;
}

int obj_get_priority (int id)
{
    return obj[id].priority;
}

void obj_set_rotation_expansion (int id, double rotation, double expansion)
{
    obj[id].rotation = rotation;
    obj[id].expansion = expansion;
}

void obj_set_position (int id, double x, double y)
{
    obj[id].x = x;
    obj[id].y = y;
}

void obj_get_location (int id, double *x, double *y, double *rotation_center_x, double *rotation_center_y,
                       double *rotation, double *expansion)
{
    *x = obj[id].x;
    *y = obj[id].y;
    *rotation_center_x = obj[id].rotation_center_x;
    *rotation_center_y = obj[id].rotation_center_y;
    *rotation = obj[id].rotation;
    *expansion = obj[id].expansion;
}

void obj_set_tilesheet_from_file (int tilesheet_id, const char *filename)
{
    g_return_if_fail (tilesheet_id < 0 || tilesheet_id >= OBJSHEET_COUNT);
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
        if (tilesheet_id == 0)
            main_pixbuf = pb;
        else if (tilesheet_id == 1)
            sub_pixbuf = pb;
        else
            abort ();
        g_debug ("loaded pixbuf %s as obj tilesheet %d", path, tilesheet_id);
        g_free (path);
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
    GdkPixbuf *pb;
    
    g_return_val_if_fail (id < 0 || id >= MAIN_OBJ_COUNT + SUB_OBJ_COUNT, NULL);
    g_return_val_if_fail (obj[id].sprite_width > 0, NULL);
    g_return_val_if_fail (obj[id].sprite_height > 0, NULL);
    
    width = obj[id].sprite_width;
    height = obj[id].sprite_height;

    if (id < MAIN_OBJ_COUNT)
        pb = main_pixbuf;
    else if (id >= MAIN_OBJ_COUNT && id < MAIN_OBJ_COUNT + SUB_OBJ_COUNT)
        pb = sub_pixbuf;
    else
        abort ();

    g_return_val_if_fail (pb != NULL, NULL);

    xgdk_pixbuf_get_width_height_stride (pb, &spritesheet_width, &spritesheet_height, &spritesheet_stride);

    g_return_val_if_fail (spritesheet_width > 0, NULL);
    g_return_val_if_fail (spritesheet_height > 0, NULL);
    g_return_val_if_fail (spritesheet_stride > 0, NULL);

    uint32_t *spritesheet_data = xgdk_pixbuf_get_argb32_pixels (pb);
    
    surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
    data = xcairo_image_surface_get_argb32_data (surf);
    stride = xcairo_image_surface_get_argb32_stride (surf);
    xcairo_surface_flush (surf);
    for (guint j = 0; j < height; j++)
    {
        for (guint i = 0; i < width; i++)
        {
            guint si, sj;
            sj = j + obj[id].spritesheet_j;
            if (obj[id].vflip == TRUE)
                sj = height - sj;
            si = i + obj[id].spritesheet_i;
            if (obj[id].hflip == TRUE)
                si = width - si;

            if (si >= spritesheet_width || sj >= spritesheet_height)
            {
                g_critical ("out of range on sprite sheet");
                c32 = 0xffff00ff;
            }
            else
                c32 = spritesheet_data[sj * spritesheet_stride + si];
            data[j * stride + i] = adjust_colorval (c32, obj[id].colorswap, obj[id].brightness);
        }
    }
    xcairo_surface_mark_dirty (surf);
    return surf;
}

SCM_DEFINE (G_obj_hide, "obj-hide", 1, 0, 0, (SCM gid), "\
Set object to not draw.")
{
    unsigned id = guile_to_ranged_uint_or_error ("obj-hide", SCM_ARG1, OBJ_COUNT, gid);
    obj_hide (id);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_obj_show, "obj-show", 1, 0, 0, (SCM gid), "\
Set object to draw.")
{
    unsigned id = guile_to_ranged_uint_or_error ("obj-hide", SCM_ARG1, OBJ_COUNT, gid);
    obj_hide (id);
    return SCM_UNSPECIFIED;   
}

void
init_guile_obj_procedures (void)
{
#include "obj.x"
  scm_c_export ("obj-hide",
                "obj-show",
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
