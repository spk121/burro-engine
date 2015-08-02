/*---------------------------------------------------------------------------------

	spritesheet.h
	Copyright (C) 2015
		Michael L. Gran (spk121)

	GPL3+
---------------------------------------------------------------------------------*/
/** @file spritesheet.h
    @brief Spritesheets for map-and-sprite backgrounds
*/

#ifndef BURRO_SPRITESHEET_H
#define BURRO_SPRITESHEET_H

#include <stdbool.h>
#include <stdint.h>
#include "vram.h"

/** The height of a map background's sprite */
#define SPRITE_HEIGHT 16
/** The width of a map background's sprite */
#define SPRITE_WIDTH 16

/** Indices for the two spritesheets. */
typedef enum spritesheet_index_tag {
    SPRITESHEET_MAIN = 0,
    SPRITESHEET_SUB = 1,
    SPRITESHEET_COUNT = 2
} spritesheet_index_t;

/** The allowed sizes of BG spritesheets, in pixels. */
typedef enum spritesheet_size_tag {
    SPRITESHEET_SIZE_32x32,              /**< 2x2 16px blocks, requires 1k uint32 VRAM */
    SPRITESHEET_SIZE_128x128,            /**< 8x8 16px blocks, requires 16k uint32 VRAM */
    SPRITESHEET_SIZE_256x256,            /**< 16x16 16px blocks, requires 64k uint32 VRAM */
    SPRITESHEET_SIZE_512x256,            /**< 32x16 16px blocks, requires 128k uint32 VRAM */
    SPRITESHEET_SIZE_256x512,            /**< 16x32 16px blocks, requires 128k uint32 VRAM */
    SPRITESHEET_SIZE_512x512,            /**< 32x32 16px blocks, requires 256k uint32 VRAM */
} spritesheet_size_t;

typedef struct spritesheet
{
    spritesheet_size_t size;
    vram_bank_t bank;
    uint32_t *storage;
    uint32_t **data;
} spritesheet_t;

/** Initializes a spritesheet's size and virtual memory.
    @param size
        size of the spritesheet, SPRITESHEET_SIZE_512x512, etc
    @param bank
        storage location for this background: VRAM_A, VRAM_B, etc
*/
void spritesheet_init (spritesheet_index_t id, spritesheet_size_t siz,
                     vram_bank_t bank);

/** Returns the 2D U32 pointer to main spritesheeet data.
 */
uint32_t **spritesheet_get_u32_data (spritesheet_index_t id);

/** Return the height, in pixels, of a spritesheet
 */
int spritesheet_get_height (spritesheet_index_t id);

/** Return the width, in pixels, of a spritesheet
 */
int spritesheet_get_width (spritesheet_index_t id);

/** Return the height, in sprites, of a spritesheet
 */
int spritesheet_get_height_in_sprites (spritesheet_index_t id);

/** Return the width, in sprites, of a spritesheet
 */
int spritesheet_get_width_in_sprites (spritesheet_index_t id);


/** Return the size, in total number of pixels, of a spritesheet
 */
size_t spritesheet_get_size (spritesheet_index_t id);

/** Initializes a spritesheet's contents from a file.
 *  Note that this doesn't change the size of a spritesheet as assigned
 *  in 'spritesheet_init'.  If the file is larger than the spritesheet,
 *  extra data is dropped.  If it is smaller, some part of the
 *  spritesheet will remain unfilled.
 *  @param id - either MAIN or SUB spritesheet index
 *  @param filename - the name of a PNG file in the data directory
 */
void spritesheet_set_data_from_file (spritesheet_index_t, const char *filename);

/** Initialize spritesheet procedures for the scripting engine
 */
void spritesheet_init_guile_procedures (void);

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


    
