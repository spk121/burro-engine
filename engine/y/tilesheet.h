/*---------------------------------------------------------------------------------

	sheet.h
	Copyright (C) 2015
		Michael L. Gran (spk121)

	GPL3+
---------------------------------------------------------------------------------*/
/** @file sheet.h
    @brief Sheets and spritesheets
*/

#ifndef BURRO_SHEET_H
#define BURRO_SHEET_H

#include <stdbool.h>
#include <stdint.h>
#include "vram.h"

/** The height of a spritesheet tile */
#define TILE_HEIGHT 16
/** The width of a spritesheet tile */
#define TILE_WIDTH 16

/** Indices for the two sheets. */
typedef enum sheet_index_tag {
    SHEET_MAIN_BG = 0,
    SHEET_SUB_BG = 1,
    SHEET_MAIN_OBJ = 2,
    SHEET_SUB_OBJ = 3,
    SHEET_COUNT = 4
} sheet_index_t;

#define heet_assert_valid_index(_x) \
    g_assert(_x >= 0 && _x < SHEET_COUNT)

typedef struct sheet
{
    matrix_size_t size;
    vram_bank_t bank;
    uint32_t *storage;
    uint32_t **data;
} sheet_t;

/** Initializes a sheet's size and virtual memory.
    @param id
        SHEET_MAIN_BG, SHEET_SUB_BG, etc
    @param size
        size of the sheet, MATRIX_SIZE_512x512, etc
    @param bank
        storage location for this background: VRAM_A, VRAM_B, etc
*/
void sheet_init (sheet_index_t id, matrix_size_t siz,
                     vram_bank_t bank);

/** Returns the 2D U32 pointer to tilesheeet data.
 */
uint32_t **sheet_get_u32_data (sheet_index_t id);

/** Returns the U32 pointer to the sheet_data
 */
uint32_t *sheet_get_u32_storage (sheet_index_t id);

/** Return the height, in pixels, of a sheet
 */
int sheet_get_height (sheet_index_t id);

/** Return the width, in pixels, of a sheet
 */
int sheet_get_width (sheet_index_t id);

/** Return the height, in tiles, of a sheet
 */
int sheet_get_height_in_tiles (sheet_index_t id);

/** Return the width, in tiles, of a sheet
 */
int sheet_get_width_in_tiles (sheet_index_t id);

/** Return the size, in total number of pixels, of a sheet
 */
size_t sheet_get_size (sheet_index_t id);

/** Initializes a sheet's contents from a file.
 *  Note that this doesn't change the size of a sheet as assigned
 *  in 'sheet_init'.  If the file is larger than the sheet,
 *  extra data is dropped.  If it is smaller, some part of the
 *  sheet will remain unfilled.
 *  @param id - either MAIN or SUB sheet index
 *  @param filename - the name of a PNG file in the data directory
 */
void sheet_set_data_from_image_file (sheet_index_t, const char *filename);
void sheet_set_data_from_csv_file (sheet_index_t, const char *filename);

/** Initialize sheet procedures for the scripting engine
 */
void sheet_init_guile_procedures (void);

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


    
