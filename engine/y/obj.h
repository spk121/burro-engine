/** @file obj.h
    Control and rendering of sprites.
*/


#ifndef BURRO_OBJ_H
#define BURRO_OBJ_H

#include "../x/xguile.h"
#include "vram.h"

enum obj_const_tag {
    MAIN_OBJ_COUNT = 128,
    SUB_OBJ_COUNT = 128,
    OBJ_COUNT = MAIN_OBJ_COUNT + SUB_OBJ_COUNT,
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

void obj_hide (int id);
void obj_show (int id);
bool obj_is_shown (int id);
void obj_init (int id, int spritesheet_i, int spritesheet_j, obj_size_t size,
		 double rotation_center_x, double rotation_center_y, bool hflip, bool vflip);
void obj_set_spritesheet_origin (int id, int spritesheet_i, int spritesheet_j);

void obj_set (int id, int priority, double x, double y, double rotation, double expansion);
void obj_set_priority (int id, int priority);
int obj_get_priority (int id);
void obj_set_rotation_expansion (int id, double rotation, double expansion);
void obj_set_position (int id, double x, double y);
void obj_get_location (int id, double *x, double *y, double *rotation_center_x, double *rotation_center_y,
			 double *rotation, double *expansion);

cairo_surface_t *obj_get_cairo_surface (int id);

/** \fn SCM FART_main_spritesheet_init(SCM size, SCM bank)
 *  \brief Set the size of the main OBJ spritesheet, and assign a pre-allocated memory buffer to it.
 *  \param [in] size - the size of the spritesheet
 *  \param [in] bank - the pre-allocated memory buffer to be assigned to it
 */

/** */
SCM G_FART (SCM x);

SCM G_obj_hide (SCM gid);
SCM G_obj_show (SCM gid);

#endif
