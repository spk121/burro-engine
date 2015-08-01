/*---------------------------------------------------------------------------------

	tilesheet.h
	Copyright (C) 2015
		Michael L. Gran (spk121)

	GPL3+
---------------------------------------------------------------------------------*/
/** @file tilesheet.h
    @brief Tilesheets for map-and-tile backgrounds
*/

#ifndef BURRO_TILESHEET_H
#define BURRO_TILESHEET_H

#include <stdbool.h>
#include <stdint.h>
#include "vram.h"

/** Indices for the two tilesheets. */
typedef enum tilesheet_index_tag {
    TILESHEET_MAIN = 0,
    TILESHEET_SUB = 1,
    TILESHEET_COUNT = 2
} tilesheet_index_t;

/** The allowed sizes of BG tilesheets, in pixels. */
typedef enum tilesheet_size_tag {
    TILESHEET_SIZE_32x32,              /**< 2x2 16px blocks, requires 1k uint32 VRAM */
    TILESHEET_SIZE_128x128,            /**< 8x8 16px blocks, requires 16k uint32 VRAM */
    TILESHEET_SIZE_256x256,            /**< 16x16 16px blocks, requires 64k uint32 VRAM */
    TILESHEET_SIZE_512x256,            /**< 32x16 16px blocks, requires 128k uint32 VRAM */
    TILESHEET_SIZE_256x512,            /**< 16x32 16px blocks, requires 128k uint32 VRAM */
    TILESHEET_SIZE_512x512,            /**< 32x32 16px blocks, requires 256k uint32 VRAM */
} tilesheet_size_t;

typedef struct tilesheet
{
    tilesheet_size_t size;
    vram_bank_t bank;
    uint32_t *storage;
    uint32_t **data;
} tilesheet_t;

/** Returns the 2D U32 pointer to main tilesheeet data.
 */
uint32_t **tilesheet_get_data_ptr (tilesheet_index_t id);

/** Initializes a tilesheet's size and virtual memory.
    @param size
        size of the tilesheet, TILESHEET_SIZE_512x512, etc
    @param bank
        storage location for this background: VRAM_A, VRAM_B, etc
*/
void tilesheet_init (tilesheet_index_t id, bg_size_t siz, vram_bank_t bank);

/** Initializes a tilesheet's contents from a file.
 *  Note that this doesn't change the size of a tilesheet as assigned
 *  in 'tilesheet_init'.  If the file is larger than the tilesheet,
 *  extra data is dropped.  If it is smaller, some part of the
 *  tilesheet will remain unfilled.
 *  @param id - either MAIN or SUB tilesheet index
 *  @param filename - the name of a PNG file in the data directory
 */
void tilesheet_set_data_from_file (tilesheet_index_t, const char *filename);

/** Initialize tilesheet procedures for the scripting engine
 */
void tilesheet_init_guile_procedures (void);

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/


    
