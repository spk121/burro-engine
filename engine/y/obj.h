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
    SPRITESHEET_COUNT = 2
  };

/** The allowed sizes of OBJ spritesheets, in pixels. */
typedef enum obj_spritesheet_size_tag {
    OBJ_SPRITESHEET_SIZE_32x32,              /**< 2x2 16px blocks, requires 1k uint32 VRAM */
    OBJ_SPRITESHEET_SIZE_128x128,            /**< 8x8 16px blocks, requires 16k uint32 VRAM */
    OBJ_SPRITESHEET_SIZE_256x256,            /**< 16x16 16px blocks, requires 64k uint32 VRAM */
    OBJ_SPRITESHEET_SIZE_512x256,            /**< 32x16 16px blocks, requires 128k uint32 VRAM, main screen only */
    OBJ_SPRITESHEET_SIZE_256x512,            /**< 16x32 16px blocks, requires 128k uint32 VRAM, main screen only */
    OBJ_SPRITESHEET_SIZE_512x512,            /**< 32x32 16px blocks, requires 256k uint32 VRAM, main screen only */
} obj_spritesheet_size_t;

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

/** Set the size of the main OBJ spritesheet, and assign a pre-allocated memory buffer to it.
 *  @param [in] size - the size of the spritesheet
 *  @param [in] bank - the pre-allocated memory buffer to be assigned to it
 */
void obj_main_spritesheet_init (obj_spritesheet_size_t size, vram_bank_t bank);

/** Set the size of the main OBJ spritesheet, and assign a pre-allocated memory buffer to it.
 *  @param [in] size - the size of the spritesheet
 *  @param [in] bank - the pre-allocated memory buffer to be assigned to it
 */
void obj_sub_spritesheet_init (obj_spritesheet_size_t size, vram_bank_t bank);

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

void obj_set_spritesheet_from_file (int tilesheet_id, const char *filename);

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
