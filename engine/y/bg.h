/*---------------------------------------------------------------------------------

	background.h -- definitions for DS backgrounds
	Copyright (C) 2013,2014
		Michael L. Gran (spk121)

	GPL3+
---------------------------------------------------------------------------------*/
/*! \file bg.h
    \brief background defines and functionality

    These are the public as well as the internal API to control backgrounds.
    There are four backgrounds for both the main and sub screens.  BG0 is 
    the one most in the foreground and BG3 is the one most in the background.

    Each background can be one of 3 different forms:
    - a 32-bit ARGB bitmap
    - an map that points to tiles in an ARGB32 bitmap
*/

#ifndef BURRO_BG_H
#define BURRO_BG_H

#include <stdbool.h>
#include <stdint.h>
#include "../x.h"

//! The number of background layers for the main screen
#define BG_MAIN_BACKGROUNDS_COUNT 4
//! The number of background layers for the sub screen
#define BG_SUB_BACKGROUNDS_COUNT 4
//! The maximum height of the map background in tiles, or bmp background in pixels
#define BG_DATA_HEIGHT 256
//! The maximum width of the map background in tiles, or bmp background in pixels
#define BG_DATA_WIDTH 256
//! The maximum height, in tiles, of a map background's tilesheet
#define BG_TILESHEET_HEIGHT_IN_TILES 64
//! The maximum width, in tiles, of a map background's tilesheet
#define BG_TILESHEET_WIDTH_IN_TILES 64
//! The height of a map background's tile
#define BG_TILE_HEIGHT 16
//! The width of a map background's tile
#define BG_TILE_WIDTH 16
//! The maximum height, in pixels, of a map background's tilesheet
#define BG_TILESHEET_HEIGHT (BG_TILESHEET_HEIGHT_IN_TILES * BG_TILE_HEIGHT)
//! The maximum width, in pixels, of a map background's tilesheet
#define BG_TILESHEET_WIDTH (BG_TILESHEET_WIDTH_IN_TILES * BG_TILE_WIDTH)

//! Enumeration of the 8 background layer IDs
enum bg_index_tag {
    BG_MAIN_0 = 0,
    BG_MAIN_1 = 1,
    BG_MAIN_2 = 2,
    BG_MAIN_3 = 3,
    BG_SUB_0 = 4,
    BG_SUB_1 = 5,
    BG_SUB_2 = 6,
    BG_SUB_3 = 7
  };

typedef enum bg_index_tag bg_index_t;

//! Allowed background types, used in bg_init
enum bg_type_tag {
    BG_TYPE_NONE,
    BG_TYPE_MAP, 
    BG_TYPE_BMP,
};

typedef enum bg_type_tag bg_type_t;

enum bg_size_tag {
    BG_SIZE_16x16,              /* 1k VRAM */
    BG_SIZE_32x16,              /* 2k VRAM */
    BG_SIZE_16x32,              /* 2k VRAM */
    BG_SIZE_32x32,              /* 4k VRAM */
    BG_SIZE_128x128,            /* 64k VRAM */
    BG_SIZE_256x256,            /* 256k VRAM */
    BG_SIZE_512x256,            /* 512k VRAM */
    BG_SIZE_256x512,            /* 512k VRAM */
    BG_SIZE_512x512,            /* 1024k VRAM */
};

typedef enum bg_size_tag bg_size_t;
    
/*! \brief Returns the pointer to the map or bmp data
 */
uint32_t *bg_get_data_ptr (bg_index_t id);

/*! \brief Returns the pointer to tilesheeet data
 */
uint32_t *bg_get_main_tilesheet_ptr (void);

/*! \brief Returns the pointer to tilesheeet data
 */
uint32_t *bg_get_sub_tilesheet_ptr (void);

/*! \brief Gets the priority of the background layer.
    \param id
        background layer id. e.g. BG_MAIN_0
    \return
        priority 0, 1, 2, or 3
*/
int bg_get_priority (int id);

/*! \brief Hides the current background
    \param id
        background layer ID. e.g. BG_MAIN_0
*/ 
void bg_hide (int id);

/*! \brief Initializes a background on either the main or sub displays
           Sets up the format and size of a background layer.  Also resets its
           rotation and scaling back to 1:1 and 0 degress of rotation.  Any
           bitmap resource associated with the background will be reset.
           The priority will be reset to 0, 1, 2 or 3 respectively.
    \param id
        background layer to init. Must be BG_MAIN_0 , 1, 2, 3 or BG_SUB_0, 1, 2, 3
    \param type
        the type of background to init: BG_TYPE_MAP, BG_TYPE_BMP
    \param size
        size of the BG, BG_SIZE_16x16, etc...
    \param bank
        storage location for this background: VRAM_A, VRAM_B, etc
*/
void bg_init (bg_index_t id, bg_type_t type, bg_size_t siz, vram_bank_t bank);

void bg_init_all_to_default ();

/*! \brief Performs a cumulative rotation of the background by the specified angle. 
    \param id
        background layer ID
    \param angle
        the angle of counter clockwise rotation in degrees
*/ 
void bg_rotate (int id, double angle);

/*! \brief Scrolls the background by the specified relative values.
         Specifically, it is a relative move of the rotational center of the
         background.
    \param id
        background layer ID, e.g. BG_MAIN_0
    \param dx
        the horizontal scroll
    \param dy
        vertical scroll
    \note
        Be mindful of the ordering of the rotation, expansion, and scroll operations
*/ 
void bg_scroll (int id, double dx, double dy);

/*! \brief Sets the rotation and scale of the background in one operation.
    \param id
        background layer ID, e.g. BG_MAIN_0
    \param rotation
        the angle of counter clockwise rotation about the rotational center in degrees
    \param expansion
        the expansion about the rotational center of the background, where 1.0 is
        no expansion
    \param scroll_x
        the offset of the rotational center of the background from the screen origin
    \param scroll_y
        the offset of the rotational center of the background from the screen origin
    \param rotation_center_x
        the pixel location of the (unrotated, unexpanded) background that will be its
        rotational center
    \param rotation_center_y
        the pixel location of the (unrotated, unexpanded) background that will be its
        rotational center
*/ 
void bg_set (int id, double rotation, double expansion, double scroll_x, double scroll_y,
	     double rotation_center_x, double rotation_center_y);

/*! \brief Sets the center of rotation of a background
    \param id
        background layer ID, e.g. BG_MAIN_0
    \param rotation_center_x
        the pixel location of the (unrotated, unexpanded) background that will be its
        rotational center
    \param rotation_center_y
        the pixel location of the (unrotated, unexpanded) background that will be its
        rotational center
*/
void bg_set_rotation_center (int id, double rotation_center_x, double rotation_center_y);

/*! \brief Sets the background priority
    \param id
        background layer id returned from bgInit or bgInitSub
    \param priority
        background priority (0-3) where 0 is most in the background and 3 is most 
        in the foreground
*/ 
void bg_set_priority (int id, int priority);

/*! \brief Performs sets the rotation of the background to specified angle. 
        This rotation is about its rotational center.
    \param id
        background layer ID
    \param rotation
        the angle of counter clockwise rotation in degrees
*/ 
void bg_set_rotation (int id, double rotation);

/*! \brief Performs sets the rotation and expansion of a background.
        This rotation and expansion is about its rotational center.
    \param id
        background layer ID
    \param rotation
        the angle of counter clockwise rotation in degrees
    \param expansion
        the expansion ratio, where 1.0 is 1:1 expansion
*/ 
void bg_set_rotation_expansion (int id, double rotation, double expansion);

/*! \brief Performs sets the expansion of a background.
        This expansion is about its rotational center.
    \param id
        background layer ID
    \param expansion
        the expansion ratio, where 1.0 is 1:1 expansion
*/ 
void bg_set_expansion (int id, double expansion);

/*! \brief Shows (makes visible) the specified background layer
    \param id
        background layer ID. e.g. BG_MAIN_0
*/ 
void bg_show (int id);

void bg_set_backdrop_color (uint32_t c32);
void bg_get_backdrop_color_rgb (double *r, double *g, double *b);



bool bg_is_shown (int id);

void bg_reset (int id, bg_type_t type);






/*! \brief Sets the map part of a BG_TYPE_MAP map-and-tile background from a resource.
        Each pixel in the map image will be interpreted as the index to an 8x8 pixel
        block in the tile image, where each block in the tile image is numbered
        row-wise sequentially.
    \param id
        background layer ID, e.g. BG_MAIN_0
    \param
        name of image resource in the GResource bundle
*/
void bg_set_map_from_resource (int id, const char *resource);

/*! \brief Sets the tilesheet of a BG_TYPE_MAP map-and-tile background from a resource.
        The tilesheet is interpreted as 8x8 pixel blocks, numbered sequentially rowwise.
    \param id
        background layer ID, e.g. BG_MAIN_0
    \param
        name of image resource in the GResource bundle
*/
void bg_set_tilesheet_from_resource (int id, const char *resource);

/*! \brief Sets the a BG_TYPE_BMP background from a resource.
    \param id
        background layer ID, e.g. BG_MAIN_0
    \param
        name of image resource in the GResource bundle
*/
void bg_set_bmp_from_file (int id, const char *filename);
void bg_set_bmp_from_resource (int id, const char *resource);

void bg_get_transform (int id, double *scroll_x, double *scroll_y, double *rotation_center_x,
		       double *rotation_center_y, double *rotation, double *expansion);
		       
cairo_surface_t *bg_render_to_cairo_surface (int id);

void bg_init_guile_procedures (void);

#endif
