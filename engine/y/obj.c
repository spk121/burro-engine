#include "../x/xcairo.h"
#include "../x/xglib.h"
#include "eng.h"
#include "obj.h"
#include "tga.h"

typedef struct obj_entry
{
    /** Sprite is visible if true */
    gboolean enable;

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

    gboolean hflip;
    gboolean vflip;

    /** The element of the palette that corresponds to this object's first color */
    int palette_offset;
} obj_entry_t;

typedef struct obj_data
{
    guint8 bmp[OBJSHEET_HEIGHT][OBJSHEET_WIDTH];
    guint32 palette[OBJSHEET_PALETTE_COLORS_COUNT];
} obj_data_t;

obj_entry_t obj[MAIN_OBJ_COUNT + SUB_OBJ_COUNT];
obj_data_t objsheet[2];

/****************************************************************/

static guint32
adjust_colorval (guint16 c16)
{
    guint32 a, r, g, b, c32;
    a = (((guint32) c16 & 0b1000000000000000) >> 15);
    r = (((guint32) c16 & 0b0111110000000000) >> 10);
    g = (((guint32) c16 & 0b0000001111100000) >> 5);
    b = ((guint32) c16 & 0b0000000000011111);
    if (eng_is_colorswap ())
    {
        double temp = r;
        r = b;
        b = temp;
    } 
    if (a > 0)
        a = 0xff;
    r = r * eng_get_brightness ();
    g = g * eng_get_brightness ();
    b = b * eng_get_brightness ();
    c32 = (a << 24) + (r << 16) + (g << 8) + b;
    return c32;
}

void obj_hide (int id)
{
    obj[id].enable = TRUE;
}

void obj_show (int id)
{
    obj[id].enable = FALSE;
}

gboolean obj_is_shown (int id)
{
    return obj[id].enable;
}

void obj_init (int id, int spritesheet_i, int spritesheet_j, int sprite_width, int sprite_height,
               double rotation_center_x, double rotation_center_y, gboolean hflip, gboolean vflip,
               int palette_offset)
{
    obj[id].spritesheet_i = spritesheet_i;
    obj[id].spritesheet_j = spritesheet_j;
    obj[id].sprite_width = sprite_width;
    obj[id].sprite_height = sprite_height;
    obj[id].rotation_center_x = rotation_center_x;
    obj[id].rotation_center_y = rotation_center_y;
    obj[id].hflip = hflip;
    obj[id].vflip = vflip;
    obj[id].palette_offset = palette_offset;
}

void obj_set_spritesheet_origin (int id, int spritesheet_i, int spritesheet_j)
{
    obj[id].spritesheet_i = spritesheet_i;
    obj[id].spritesheet_j = spritesheet_j;
}

void obj_set (int id, int priority, double x, double y, double rotation, double expansion,
              int palette_offset)
{
    obj[id].priority = priority;
    obj[id].x = x;
    obj[id].y = y;
    obj[id].rotation = rotation;
    obj[id].expansion = expansion;
    obj[id].palette_offset = palette_offset;
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

void obj_set_palette_offset (int id, int offset)
{
    obj[id].palette_offset = offset;
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

cairo_surface_t *obj_render_to_cairo_surface (int id)
{
    guint width, height, stride;
    guint32 *data;
    guint16 c16;
    guint8 index;
    cairo_surface_t *surf;
    width = obj[id].sprite_width;
    height = obj[id].sprite_height;

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
            if (id < MAIN_OBJ_COUNT)
            {
                index = objsheet[0].bmp[sj][si];
                c16 = objsheet[0].palette[index + obj[id].palette_offset];
            }
            else
            {
                index = objsheet[1].bmp[sj][si];
                c16 = objsheet[1].palette[index + obj[id].palette_offset];
            }
            data[j * stride + i] = adjust_colorval (c16);
        }
    }
    xcairo_surface_mark_dirty (surf);
    return surf;
}
