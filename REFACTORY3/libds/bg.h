#ifndef LIBDS_BG_H
#define LIBDS_BG_H

#include <stdbool.h>
#include <stdint.h>

/*----------------------------------------------------------------------------

	bg.h -- definitions for DS backgrounds
	Copyright (C) 2013,2014
		Michael L. Gran (spk121)

	GPL3+
-----------------------------------------------------------------------------*/
/*! \file bg.h
    \brief background defines and functionality

    Each background can be one of 3 different forms:
    - a 16-bit ARGB1555 bitmap
    - an 8-bit indexed bitmap with a 15-bit RGB555 colormap where color #0 is
      always transparent.
    - an 8-bit map that points to 8 by 8 pixel tiles in a separate 8-bit
      indexed bitmap with a 15-bit RGB555 colormap
*/

//! The maximum height of the map background, in tiles
#define BG_MAP_HEIGHT_MAX 512
//! The maximum width, in tiles, of the map background
#define BG_MAP_WIDTH_MAX 512
//! The maximum height, in pixels, of a map background's tilesheet
#define BG_TILESHEET_HEIGHT 256
//! The maximum width, in pixels, of a map background's tilesheet
#define BG_TILESHEET_WIDTH 256
//! The height of a map background's tile
#define BG_TILE_HEIGHT 8
//! The width of a map background's tile
#define BG_TILE_WIDTH 8
//! The maximum height, in tiles, of a map background's tilesheet
#define BG_TILESHEET_HEIGHT_IN_TILES (BG_TILESHEET_HEIGHT/BG_TILE_HEIGHT)
//! The maximum width, in tiles, of a map background's tilesheet
#define BG_TILESHEET_WIDTH_IN_TILES (BG_TILESHEET_WIDTH/BG_TILE_WIDTH)
//! The number of colors for a map background's tilesheet or a BMP8 background
#define BG_PALETTE_COLORS_COUNT_MAX 256
//! The maximum height of a BMP8 background, in pixels
#define BG_BMP8_HEIGHT_MAX 512
//! The maximum width of a BMP8 background, in pixels
#define BG_BMP8_WIDTH_MAX 512
//! The maximum height of a BMP16 background, in pixels
#define BG_BMP16_HEIGHT_MAX 512
//! The maximum width of a BMP16 background, in pixels
#define BG_BMP16_WIDTH_MAX 512

//! Enumeration of the 8 background layer IDs
enum ds_bg_index_impl {
    BG_MAIN_0 = 0,
    BG_MAIN_1 = 1,
    BG_MAIN_2 = 2,
    BG_MAIN_3 = 3,
    BG_SUB_0 = 4,
    BG_SUB_1 = 5,
    BG_SUB_2 = 6,
    BG_SUB_3 = 7
  };

typedef enum bg_index_impl ds_bg_index_t;

//! Allowed background types, used in ds_bg_init
enum ds_bg_type_impl {
    BG_TYPE_MAP, //!< 8bpp Tiled background with 16 bit tile indexes
		 //!and no allowed rotation or scaling
    BG_TYPE_BMP8, //!< Bitmap background with 8 bit color values which
		  //!index into a 256-color RGB555 palette, where
		  //!color #0 is transparent
    BG_TYPE_BMP16, //!< Bitmap background with 16 bit color values of
		   //!the form ABGR1555
};

typedef enum ds_bg_type_impl ds_bg_type_t;

struct ds_bg_map_data
{
    int height;
    int width;
    uint16_t map[BG_MAP_HEIGHT_MAX][BG_MAP_WIDTH_MAX];
    uint8_t tilesheet[BG_TILESHEET_HEIGHT][BG_TILESHEET_WIDTH];
    uint16_t palette[BG_PALETTE_COLORS_COUNT_MAX];
};


struct ds_bg_bmp8_data
{
    int height;
    int width;
    uint8_t bmp[BG_BMP8_HEIGHT_MAX][BG_BMP8_WIDTH_MAX];
    uint16_t palette[BG_PALETTE_COLORS_COUNT_MAX];
};

struct ds_bg_bmp16_data
{
    int height;
    int width;
    uint16_t bmp[BG_BMP16_HEIGHT_MAX][BG_BMP16_WIDTH_MAX];
};


typedef struct ds_bg_impl
{
    /* BG is display when true */
    bool enable;

    /** tile and map, palette bmp, or true color bmp */
    ds_bg_type_t type;

    /* z-level: 0 is foreground, 3 is background */
    int priority;

    /** the "user" or screen location of the rotation center of the
	background */
    double scroll_x, scroll_y;

    /** the "device" location of the rotation center of the background*/
    double rotation_center_x, rotation_center_y;

    /** the expansion factor of the background: 1.0 = 1 pixel per pixel */
    double expansion;

    /** the rotation angle of the background about its rotation
	center, in radians */
    double rotation;

    union
    {
        struct ds_bg_map_data map;
        struct ds_bg_bmp8_data bmp8;
        struct ds_bg_bmp16_data bmp16;
    };
} ds_bg_t;

#if 0
void bg_set_backdrop_color (guint16 c16);
void bg_get_backdrop_color_rgb (double *r, double *g, double *b);

guint16 *bg_get_map_ptr (int id);
guint8 *bg_get_tilesheet_ptr (int id);
guint8 *bg_get_bmp8_ptr (int id);
guint16 *bg_get_bmp16_ptr (int id);

/*! \brief Gets the priority of the background layer.
    \param id
        background layer id. e.g. BG_MAIN_0
    \return
        priority 0, 1, 2, or 3
*/
int bg_get_priority (int id);

gboolean bg_is_shown (int id);

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
        the type of background to init: BG_TYPE_MAP, BG_TYPE_BMP8 or BG_TYPE_BMP16
    \param width
        width of the background in pixels. For BG_TYPE_MAP, must be divisible by 8.
    \param height
        height of the background in pixels. For BG_TYPE_MAP, must be divisible by 8.
*/
void bg_init (int id, bg_type_t type, guint width, guint height);

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

/*! \brief Sets the a BG_TYPE_BMP8 background from a resource.
    \param id
        background layer ID, e.g. BG_MAIN_0
    \param
        name of image resource in the GResource bundle
*/
void bg_set_bmp8_from_resource (int id, const char *resource);


/*! \brief Sets the a BG_TYPE_BMP16 background from a resource.
    \param id
        background layer ID, e.g. BG_MAIN_0
    \param
        name of image resource in the GResource bundle
*/
void bg_set_bmp16_from_resource (int id, const char *resource);
void bg_get_transform (int id, double *scroll_x, double *scroll_y, double *rotation_center_x,
		       double *rotation_center_y, double *rotation, double *expansion);

cairo_surface_t *bg_render_to_cairo_surface (int id);


struct bg_impl {
    /* BG is display when true */
    bool enable;

    /** tile and map, palette bmp, or true color bmp */
    bg_type_t type;

    /* z-level: 0 is foreground, 3 is background */
    int priority;

    /** the "user" or screen location of the rotation center of the background */
    double scroll_x, scroll_y;

    /** the "device" location of the rotation center of the background*/
    double rotation_center_x, rotation_center_y;

    /** the expansion factor of the background: 1.0 = 1 pixel per pixel */
    double expansion;

    /** the rotation angle of the background about its rotation center, in radians */
    double rotation;

    union
    {
        struct bg_map_data map;
        struct bg_bmp8_data bmp8;
        struct bg_bmp16_data bmp16;
    };
} ds_bg_t;


#endif
#endif
