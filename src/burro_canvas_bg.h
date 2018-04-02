#ifndef BURRO_CANVAS_BG_H
#define BURRO_CANVAS_BG_H
/*  bg.h

    Copyright (C) 2013, 2014, 2018   Michael L. Gran
    This file is part of Burro Engine

    Burro Engine is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Burro Engine is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Burro Engine.  If not, see <http://www.gnu.org/licenses/>.
*/

/** @file bg.h
    @brief background defines and functionality

    These are the public as well as the internal API to control backgrounds.
    There are four backgrounds for both the main and sub screens.  BG0 is 
    the one most in the foreground and BG3 is the one most in the background.

    Each background can be one of 3 different forms:
    - a 32-bit ARGB bitmap
    - an map that points to tiles in an ARGB32 bitmap
*/

#ifndef BURRO_BG_H
#define BURRO_BG_H

#include <gtk/gtk.h>
#include <libguile.h>
/* #include <stdbool.h> */
/* #include <stdint.h> */
/* #include "../x.h" */
/* #include "matrix.h" */
#include "burro_canvas_vram.h"

/** The number of background layers for the main screen */
#define BG_BACKGROUNDS_COUNT 4

/** Enumeration of the 4 background layer IDs */
typedef enum {
    BG_0 = 0,
    BG_1 = 1,
    BG_2 = 2,
    BG_3 = 3,
} bg_index_t;


/** Allowed background types, used in bg_init */
typedef enum {
    BG_TYPE_NONE,
    BG_TYPE_MAP, 
    BG_TYPE_BMP,
} bg_type_t;

gboolean bg_validate_int_as_bg_index_t (int x);
gboolean bg_validate_int_as_bg_type_t (int x);
const char *bg_get_index_name (bg_index_t index);

/** Initializes the BG subsystem.
 */
void bg_init (void);

/**  Initializes a memory store for a background layers.
    @param id
        background layer to init. Must be BG_0 , 1, 2, 3
    @param size
        size of the BG, MATRIX_16x16, etc
    @param bank
        storage location for this background: VRAM_A, VRAM_B, etc
*/
// void bg_assign_memory (bg_index_t id, matrix_size_t siz, vram_bank_t bank);

/**  Returns the pointer to the map or bmp data
 */
const uint32_t *bg_get_data_ptr (bg_index_t id);

/**  Gets the priority of the background layer.
    @param id
        background layer id. e.g. BG_0
    \return
        priority 0, 1, 2, or 3
*/
int bg_get_priority (bg_index_t id);

/**  Hides the current background
    @param id
        background layer ID. e.g. BG_0
*/ 
void bg_hide (bg_index_t id);

/**  Performs a cumulative rotation of the background by the specified angle. 
    @param id
        background layer ID
    @param angle
        the angle of counter clockwise rotation in degrees
*/ 
void bg_rotate (bg_index_t id, double angle);

/**  Scrolls the background by the specified relative values.
         Specifically, it is a relative move of the rotational center of the
         background.
    @param id
        background layer ID, e.g. BG_0
    @param dx
        the horizontal scroll
    @param dy
        vertical scroll
    \note
        Be mindful of the ordering of the rotation, expansion, and scroll operations
*/ 
void bg_scroll (bg_index_t id, double dx, double dy);

/**  Sets the rotation and scale of the background in one operation.
    @param id
        background layer ID, e.g. BG_0
    @param rotation
        the angle of counter clockwise rotation about the rotational center in degrees
    @param expansion
        the expansion about the rotational center of the background, where 1.0 is
        no expansion
    @param scroll_x
        the offset of the rotational center of the background from the screen origin
    @param scroll_y
        the offset of the rotational center of the background from the screen origin
    @param rotation_center_x
        the pixel location of the (unrotated, unexpanded) background that will be its
        rotational center
    @param rotation_center_y
        the pixel location of the (unrotated, unexpanded) background that will be its
        rotational center
*/ 
void bg_set (bg_index_t id, double rotation, double expansion, double scroll_x, double scroll_y,
	     double rotation_center_x, double rotation_center_y);

/**  Sets the center of rotation of a background
    @param id
        background layer ID, e.g. BG_0
    @param rotation_center_x
        the pixel location of the (unrotated, unexpanded) background that will be its
        rotational center
    @param rotation_center_y
        the pixel location of the (unrotated, unexpanded) background that will be its
        rotational center
*/
void bg_set_rotation_center (bg_index_t id, double rotation_center_x, double rotation_center_y);

/**  Sets the background priority
    @param id
        background layer id returned from bgInit or bgInitSub
    @param priority
        background priority (0-3) where 0 is most in the background and 3 is most 
        in the foreground
*/ 
void bg_set_priority (bg_index_t id, int priority);

/**  Performs sets the rotation of the background to specified angle. 
        This rotation is about its rotational center.
    @param id
        background layer ID
    @param rotation
        the angle of counter clockwise rotation in degrees
*/ 
void bg_set_rotation (bg_index_t id, double rotation);

/**  Performs sets the rotation and expansion of a background.
        This rotation and expansion is about its rotational center.
    @param id
        background layer ID
    @param rotation
        the angle of counter clockwise rotation in degrees
    @param expansion
        the expansion ratio, where 1.0 is 1:1 expansion
*/ 
void bg_set_rotation_expansion (bg_index_t id, double rotation, double expansion);

/**  Performs sets the expansion of a background.
        This expansion is about its rotational center.
    @param id
        background layer ID
    @param expansion
        the expansion ratio, where 1.0 is 1:1 expansion
*/ 
void bg_set_expansion (bg_index_t id, double expansion);

/**  Shows (makes visible) the specified background layer
    @param id
        background layer ID. e.g. BG_0
*/ 
void bg_show (bg_index_t id);

gboolean bg_is_shown (bg_index_t id);

void bg_reset (bg_index_t id);



void bg_get_transform (bg_index_t id, double *scroll_x, double *scroll_y,
                       double *rotation_center_x, double *rotation_center_y,
                       double *rotation, double *expansion);
		       
cairo_surface_t *bg_get_cairo_surface (bg_index_t id);

gboolean bg_is_dirty(int z);

void burro_canvas_bg_init_guile_procedures (void);
void bg_fini(void);
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


#endif
