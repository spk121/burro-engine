#include <stdint.h>
#include <stdbool.h>
#include "../x.h"
#include "eng.h"
#include "guile.h"
#include "obj.h"
#include "pixbuf.h"



SCM obj_tag;
SCM_GLOBAL_VARIABLE_INIT (G_obj_display_list, "%obj-display-list", SCM_EOL);

void
obj_init_guile_type()
{
    obj_tag = scm_make_foreign_object_type (scm_from_utf8_symbol ("obj"),
                                            scm_list_2 (scm_from_utf8_symbol ("data"),
                                                        scm_from_utf8_symbol ("pixbuf")),
                                            NULL);
}

SCM obj_make (SCM pixbuf, int sheet_i, int sheet_j, int width, int height,
              double rotation_center_x, double rotation_center_y, bool hflip, bool vflip)
{
    obj_t *o = scm_gc_malloc_pointerless (sizeof (obj_t), "obj");
    memset (o, 0, sizeof(obj_t));
    o->priority = 0;
    o->sheet_i = sheet_i;
    o->sheet_i = sheet_j;
    o->width = width;
    o->height = height;
    o->rotation_center_x = rotation_center_x;
    o->rotation_center_y = rotation_center_y;
    o->expansion = 1.0;
    o->rotation = 0.0;
    o->hflip = hflip;
    o->vflip = vflip;
    o->colorswap = false;
    o->brightness = 1.0;
 
    return scm_make_foreign_object_2 (obj_tag, o, pixbuf);
}

SCM_DEFINE (G_make_obj, "make-obj", 9, 0, 0,
            (SCM s_pixbuf, SCM s_sheet_i, SCM s_sheet_j, SCM s_width, SCM s_height,
             SCM s_rotation_center_x, SCM s_rotation_center_y, SCM s_hflip, SCM s_vflip), "\
Create an OBJ, which is screen coordinates of a sprite and its associated\n\
pixel data.")
{
    SCM_ASSERT_TYPE (SCM_IS_A_P (s_pixbuf, pixbuf_tag), s_pixbuf, SCM_ARG1, "make-obj", "pixbuf");
    SCM_ASSERT_TYPE (scm_is_integer (s_sheet_i), s_sheet_i, SCM_ARG2, "make-obj", "integer");
    SCM_ASSERT_TYPE (scm_is_integer (s_sheet_j), s_sheet_j, SCM_ARG3, "make-obj", "integer");
    SCM_ASSERT_TYPE (scm_is_integer (s_width), s_width, SCM_ARG4, "make-obj", "integer");
    SCM_ASSERT_TYPE (scm_is_integer (s_height), s_height, SCM_ARG5, "make-obj", "integer");
    SCM_ASSERT_TYPE (scm_is_real (s_rotation_center_x), s_rotation_center_x, SCM_ARG6, "make-obj", "real");
    SCM_ASSERT_TYPE (scm_is_real (s_rotation_center_y), s_rotation_center_y, SCM_ARG7, "make-obj", "real");
    SCM_ASSERT_TYPE (scm_is_bool (s_hflip), s_hflip, 8, "make-obj", "boolean");
    SCM_ASSERT_TYPE (scm_is_bool (s_vflip), s_vflip, 8, "make-obj", "boolean");

    // FIXME: range check sprite coordinates to make sure they are actually valid pixbuf locations.
    return obj_make (s_pixbuf,
                     scm_to_int (s_sheet_i),
                     scm_to_int (s_sheet_j),
                     scm_to_int (s_width),
                     scm_to_int (s_height),
                     scm_to_double (s_rotation_center_x),
                     scm_to_double (s_rotation_center_y),
                     scm_to_bool (s_hflip),
                     scm_to_bool (s_vflip));
}

void obj_show (SCM s_obj)
{
    // To "show" an object, we add it to the object display list.
    SCM obj_display_list = scm_variable_ref (G_obj_display_list);
    if (scm_is_false (scm_memq (s_obj, obj_display_list)))
    {
        obj_display_list = scm_append_x (scm_list_2 (obj_display_list,
                                                     scm_list_1 (s_obj)));
    }
    scm_variable_set_x (G_obj_display_list, obj_display_list);
}

SCM_DEFINE (G_obj_show, "obj-show", 1, 0, 0, (SCM s_obj), "\
Make the object visible.")
{
    SCM_ASSERT_TYPE (SCM_IS_A_P (s_obj, obj_tag), s_obj, SCM_ARG1, "obj-show", "obj");
    obj_show (s_obj);
    return SCM_UNSPECIFIED;
}

void obj_hide (SCM s_obj)
{
    // To "hide" an object, we remove it from the object display list.
    SCM obj_display_list = scm_variable_ref (G_obj_display_list);
    SCM memlist = scm_memq (s_obj, obj_display_list);
    if (!scm_is_false (memlist))
    {
        obj_display_list = scm_delq (s_obj, obj_display_list);
        scm_variable_set_x (G_obj_display_list, obj_display_list);
    }
}

SCM_DEFINE (G_obj_hide, "obj-hide", 1, 0, 0, (SCM s_obj), "\
Make the object invisible.")
{
    SCM_ASSERT_TYPE (SCM_IS_A_P (s_obj, obj_tag), s_obj, SCM_ARG1, "obj-hide", "obj");
    obj_hide (s_obj);
    return SCM_UNSPECIFIED;
}

bool obj_is_shown (SCM s_obj)
{
    SCM obj_display_list = scm_variable_ref (G_obj_display_list);
    SCM memlist = scm_memq (s_obj, obj_display_list);
    return !scm_is_false (memlist);
}

SCM_DEFINE (G_obj_shown_p, "obj-shown?", 1, 0, 0, (SCM s_obj), "\
Return #t if the object is visible.")
{
    SCM_ASSERT_TYPE (SCM_IS_A_P (s_obj, obj_tag), s_obj, SCM_ARG1, "obj-shown?", "obj");
    return scm_from_bool (obj_is_shown (s_obj));
}

void obj_set_sheet_origin (SCM s_obj, int sheet_i, int sheet_j)
{
    obj_t *o = scm_foreign_object_ref (s_obj, 0);
    o->sheet_i = sheet_i;
    o->sheet_j = sheet_j;
}

SCM_DEFINE (G_obj_set_sheet_origin_x, "obj-set-sheet-origin!", 3, 0, 0, (SCM s_obj, SCM s_sheet_i, SCM s_sheet_j), "\
Sets the sprite image to the pixel data that begins at the given location\n\
on the obj's associated pixbuf.")
{
    SCM_ASSERT_TYPE (SCM_IS_A_P (s_obj, obj_tag), s_obj, SCM_ARG1, "obj-set-sheet-origin!", "obj");
    SCM_ASSERT_TYPE (scm_is_integer (s_sheet_i), s_sheet_i, SCM_ARG2, "obj-set-sheet-origin!", "obj");
    SCM_ASSERT_TYPE (scm_is_integer (s_sheet_j), s_sheet_j, SCM_ARG3, "obj-set-sheet-origin!", "obj");
    
    // FIXME: range checks.
    obj_set_sheet_origin (s_obj,
                          scm_to_int (s_sheet_i),
                          scm_to_int (s_sheet_j));
    return SCM_UNSPECIFIED;
}

void obj_set (SCM s_obj, int priority, double x, double y, double rot, double exp)
{
    obj_t *o = scm_foreign_object_ref (s_obj, 0);
    o->priority = priority;
    o->x = x;
    o->y = y;
    o->rotation = rot;
    o->expansion = exp;
}

SCM_DEFINE (G_obj_set, "obj-set!", 6, 0, 0, (SCM s_obj, SCM priority, SCM x, SCM y, SCM rot, SCM exp),"\
Modifiy the the position, rotation, expansion, and z-level of an OBJ.")
{
    // FIXME: typecheck
    obj_set (s_obj,
             scm_to_int (priority),
             scm_to_double (x),
             scm_to_double (y),
             scm_to_double (rot),
             scm_to_double (exp));
    return SCM_UNSPECIFIED;
}

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

cairo_surface_t *obj_render_to_cairo_surface (SCM obj)
{
    // guint width, height
    guint stride;
    uint32_t *data, c32;
    cairo_surface_t *surf;
    
    g_return_val_if_fail (SCM_IS_A_P (obj, obj_tag), NULL);

    obj_t *o = scm_foreign_object_ref (obj, 0);
    pixbuf_t *pb = scm_foreign_object_ref (scm_foreign_object_ref (obj, 1), 0);

    surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, o->width, o->height);
    data = xcairo_image_surface_get_argb32_data (surf);
    stride = xcairo_image_surface_get_argb32_stride (surf);
    xcairo_surface_flush (surf);

    if ((o->vflip == FALSE)
        && (o->hflip == FALSE)
        && (o->brightness == 1.0)
        && (o->colorswap == false))
    {
        // FASTER PATH
        for (unsigned j = 0; j < o->height; j ++)
        {
                memcpy (data + j * stride,
                        &(pb->data[(o->sheet_j + j) * pb->stride + o->sheet_i]),
                        o->width * sizeof (uint32_t));
        }
    }
    else
    {
        // SLOW PATH
        for (guint j = 0; j < o->height; j++)
        {
            for (guint i = 0; i < o->width; i++)
            {
                guint si, sj;
                sj = j + o->sheet_j;
                if (o->vflip == TRUE)
                    sj = o->height - sj;
                si = i + o->sheet_i;
                if (o->hflip == TRUE)
                    si = o->width - si;

                c32 = pb->data[sj * pb->stride + si];
                    
                data[j * stride + i] = adjust_colorval (c32, o->colorswap, o->brightness);
            }
        }
    }
    xcairo_surface_mark_dirty (surf);
    return surf;
}

int obj_get_priority (SCM s_obj)
{
    obj_t *o = scm_foreign_object_ref (s_obj, 0);
    return o->priority;
}

void obj_get_location (SCM s_obj, double *x, double *y, double *rotation_center_x, double *rotation_center_y,
                       double *rotation, double *expansion)
{
    obj_t *o = scm_foreign_object_ref (s_obj, 0);
    *x = o->x;
    *y = o->y;
    *rotation_center_x = o->rotation_center_x;
    *rotation_center_y = o->rotation_center_y;
    *rotation = o->rotation;
    *expansion = o->expansion;
}

////////////////////////////////////////////////////////////////

void
obj_init_guile_procedures (void)
{
    obj_init_guile_type();

#include "obj.x"
    scm_c_export ("make-obj",
                  "obj-show",
                  "obj-hide",
                  "obj-shown?",
                  "obj-set-sheet-origin!",
                  "obj-set!",
                  "%obj-display-list",
                  NULL);
}

#if 0

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

    /** The pixbuf that holds the sprite sheet */
    SCM sheet;

    /** location of top-left corner of sprite in sprite sheet */
    int sheet_i, sheet_j;

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
    obj_entry_t obj[MAIN_OBJ_COUNT];
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



cairo_surface_t *obj_render_to_cairo_surface (int id)
{
    guint width, height, stride;
    uint32_t *data, c32;
    cairo_surface_t *surf;
    int sheet_width, sheet_height, sheet_stride;
    int sheet_id;
    
    g_return_val_if_fail (id >= 0 || id < MAIN_OBJ_COUNT, NULL);

    width = obj_matrix_get_width (obj.obj[id].size);
    height = obj_matrix_get_height (obj.obj[id].size);

    if (id < MAIN_OBJ_COUNT)
        sheet_id = 0;
    else if (id >= MAIN_OBJ_COUNT && id < MAIN_OBJ_COUNT)
        sheet_id = 1;
    else
        abort ();

    surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
    data = xcairo_image_surface_get_argb32_data (surf);
    stride = xcairo_image_surface_get_argb32_stride (surf);
    xcairo_surface_flush (surf);

    pixbuf_t *pb = scm_foreign_object_ref (obj.obj[id].sheet, 0);
    
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
                        &(pb->data[(obj.obj[id].sheet_j + j) * pb->stride + obj.obj[id].sheet_i]),
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
                sj = j + obj.obj[id].sheet_j;
                if (obj.obj[id].vflip == TRUE)
                    sj = height - sj;
                si = i + obj.obj[id].sheet_i;
                if (obj.obj[id].hflip == TRUE)
                    si = width - si;

                c32 = pb->data[sj * pb->stride + si];
                    
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


void
obj_init_guile_procedures (void)
{
#include "obj.x"
    scm_c_export (
        "obj-init",

        "obj-hide",
        "obj-show",

        "obj-set",
        "obj-rotate",
        "obj-move",
        "obj-set-position",
        "obj-set-expansion",
        "obj-set-rotation",
        "obj-set-rotation-center",
        "obj-set-rotation-expansion",

        "obj-set-colorswap",
        "obj-set-brightness",

        "obj-set-sheet-origin",

        "obj-get-width",
        "obj-get-height",
        NULL);
}
#endif

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
