/** @file obj.h
    Control and rendering of sprites.

*/


#ifndef BURRO_OBJ_H
#define BURRO_OBJ_H

#include "../x/xguile.h"

typedef struct obj_tag
{
    /** priority aka z-level. 0 to 3 where 0 is foreground */
    int priority;

    /** location of top-left corner of sprite in sprite sheet */
    int sheet_i, sheet_j;

    /** size of sprite in pixels */
    int width, height;

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
} obj_t;


/** A reference to the OBJ foreign object type. */
extern SCM obj_tag;
extern SCM G_obj_display_list;

SCM obj_make (SCM pixbuf, int sheet_i, int sheet_j, int width, int height,
              double rotation_center_x, double rotation_center_y, bool hflip, bool vflip);
SCM G_make_obj (SCM s_pixbuf, SCM s_sheet_i, SCM s_sheet_j, SCM s_width, SCM s_height,
                SCM s_rotation_center_x, SCM s_rotation_center_y, SCM s_hflip, SCM s_vflip);
void obj_show (SCM s_obj);
SCM G_obj_show (SCM s_obj);
void obj_hide (SCM s_obj);
SCM G_obj_hide (SCM s_obj);
bool obj_is_shown (SCM s_obj);
SCM g_obj_shown_p (SCM s_obj);
void obj_set (SCM obj, int priority, double x, double y, double rotation, double expansion);
SCM G_obj_set (SCM obj, SCM priority, SCM x, SCM y, SCM rotation, SCM expansion);
int obj_get_priority (SCM s_obj);
void obj_get_location (SCM s_obj, double *x, double *y, double *rotation_center_x, double *rotation_center_y,
                       double *rotation, double *expansion);
cairo_surface_t *obj_render_to_cairo_surface (SCM obj);

void obj_init_guile_procedures();

#if 0
enum obj_const_tag {
    MAIN_OBJ_COUNT = 128,
    OBJ_COUNT = MAIN_OBJ_COUNT,
  };

/** The allowed sizes of an individual sprite, in pixels. */
typedef enum obj_sprite_size_tag {
    OBJ_SIZE_16x16,             /**< 1x1 16px block  */
    OBJ_SIZE_16x32,             /**< 1x2 16px blocks  */
    OBJ_SIZE_16x64,             /**< 1x4 16px blocks  */
    OBJ_SIZE_32x16,             /**< 2x1 16px blocks  */
    OBJ_SIZE_32x32,             /**< 2x2 16px blocks  */
    OBJ_SIZE_32x64,             /**< 2x4 16px blocks  */
    OBJ_SIZE_64x16,             /**< 4x1 16px blocks  */
    OBJ_SIZE_64x32,             /**< 4x2 16px blocks  */
    OBJ_SIZE_64x64,             /**< 4x4 16px blocks  */
    OBJ_SIZE_64x128,            /**< 4x8 16px blocks  */
    OBJ_SIZE_128x64,            /**< 8x4 16px blocks  */
    OBJ_SIZE_128x128,           /**< 8x8 16px blocks  */
} obj_size_t;

void obj_set_colorswap (bool swap);
void obj_set_brightness (double brightness);

void obj_init (int id, SCM sheet_pixbuf, int spritesheet_i, int spritesheet_j, obj_size_t size,
		 double rotation_center_x, double rotation_center_y, bool hflip, bool vflip);
void obj_set_spritesheet_origin (int id, int spritesheet_i, int spritesheet_j);

void obj_set_priority (int id, int priority);
int obj_get_priority (int id);
void obj_set_rotation_expansion (int id, double rotation, double expansion);
void obj_set_position (int id, double x, double y);

cairo_surface_t *obj_get_cairo_surface (int id);

/** \fn SCM FART_main_spritesheet_init(SCM size, SCM bank)
 *  \brief Set the size of the main OBJ spritesheet, and assign a pre-allocated memory buffer to it.
 *  \param [in] size - the size of the spritesheet
 *  \param [in] bank - the pre-allocated memory buffer to be assigned to it
 */

/** */

void obj_init_guile_procedures (void);
#endif
#endif
