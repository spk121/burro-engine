/* tga.hpp -- Targa functions

   Copyright 2014, Michael L. Gran

   This file is part of the Project Burro game engine.

   Project Burro is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   Project Burro is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Project Burro.  If not, see
   <http://www.gnu.org/licenses/>. */

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <SDL.h>

#include <string.h>
#include "tga.hpp"
#include "xsdl.hpp"

#define TGA_PACKED_HEADER_LEN (18)

#define TGA_COLOR_MAP_TYPE_NO_COLOR_MAP (0)
#define TGA_COLOR_MAP_TYPE_COLOR_MAP (1)

#define TGA_COLOR_MAP_TYPE_VALID(T)                               \
	(T->header.color_map_type == TGA_COLOR_MAP_TYPE_NO_COLOR_MAP	\
	 || T->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP)

#define BIT_TO_BYTE(n) (((n)+7)/8)

typedef enum tga_image_type_tag {
    TGA_IMAGE_TYPE_NO_IMAGE_DATA = 0,
    TGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED = 1,
    TGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR = 2,
    TGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE = 3,
    TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED = 9,
    TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR = 10,
    TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE = 11
} tga_image_type_t;

#define TGA_IMAGE_TYPE_VALID(T)										\
	(T->header.image_type == TGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED	\
	 || T->header.image_type == TGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR \
	 || T->header.image_type == TGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE \
	 || T->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED \
	 || T->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR \
	 || T->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)

#define TGA_IMAGE_TYPE_COLOR_MAPPED(T)								\
	(T->header.image_type == TGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED	\
	 || T->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED)

#define TGA_IMAGE_TYPE_TRUE_COLOR(T)									\
	(T->header.image_type == TGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR	\
	 || T->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR)

#define TGA_IMAGE_TYPE_BLACK_AND_WHITE(T)								\
	(T->header.image_type == TGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE \
	 || T->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)

#define TGA_IMAGE_TYPE_UNCOMPRESSED(T) \
	(T->header.image_type == TGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED	\
	 || T->header.image_type == TGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR \
	 || T->header.image_type == TGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE)

#define TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED(T)							\
	(T->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED \
	 || T->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR \
	 || T->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)

#define TGA_COLOR_SIZE_15_BITS (15)
#define TGA_COLOR_SIZE_16_BITS (16)
#define TGA_COLOR_SIZE_24_BITS (24)
#define TGA_COLOR_SIZE_32_BITS (32)

#define TGA_COLOR_MAP_ENTRY_SIZE_VALID(T)						  \
	((T->header.color_map_entry_size == TGA_COLOR_SIZE_15_BITS)	 \
	 || (T->header.color_map_entry_size == TGA_COLOR_SIZE_16_BITS) \
	 || (T->header.color_map_entry_size == TGA_COLOR_SIZE_24_BITS)	\
	 || (T->header.color_map_entry_size == TGA_COLOR_SIZE_32_BITS))

#define TGA_PIXEL_DEPTH_VALID(T)									 \
	((T->header.pixel_depth == TGA_COLOR_SIZE_15_BITS)	 \
	 || (T->header.pixel_depth == TGA_COLOR_SIZE_16_BITS) \
	 || (T->header.pixel_depth == TGA_COLOR_SIZE_24_BITS)	\
	 || (T->header.pixel_depth == TGA_COLOR_SIZE_32_BITS))

#define TGA_EXTENSION_SIZE (495)

#define TGA_ALPHA_ATTRIBUTES_NO_ALPHA (0)
#define TGA_ALPHA_ATTRIBUTES_UNDEFINED_IGNORED (1)
#define TGA_ALPHA_ATTRIBUTES_UNDEFINED_RETAINED (2)
#define TGA_ALPHA_ATTRIBUTES_ALPHA (3)
#define TGA_ALPHA_ATTRIBUTES_ALPHA_PREMULTIPLIED (4)

#define TGA_ALPHA_ATTRIBUTES_VALID(T) \
	((T->extension.alpha_attribute == TGA_ALPHA_ATTRIBUTES_NO_ALPHA)	\
	 || (T->extension.alpha_attribute == TGA_ALPHA_ATTRIBUTES_UNDEFINED_IGNORED) \
	 || (T->extension.alpha_attribute == TGA_ALPHA_ATTRIBUTES_UNDEFINED_RETAINED) \
	 || (T->extension.alpha_attribute == TGA_ALPHA_ATTRIBUTES_ALPHA)	\
	 || (T->extension.alpha_attribute == TGA_ALPHA_ATTRIBUTES_ALPHA_PREMULTIPLIED))

#define TGA_IMAGE_SIZE(T)											\
	((size_t) T->header.image_width * (size_t) T->header.image_height)

typedef enum tga_error_tag
{
    // Generic OK
    TGA_OK = 0,
    // Generic ERROR
    TGA_ERROR = 1,
    // Parsing error list begins here
    TGA_HEADER_TOO_SHORT,
    TGA_READ_ERROR,
    TGA_OUT_OF_MEMORY,
    TGA_COLOR_MAP_TOO_SHORT,
    TGA_COLOR_MAP_TYPE_OUT_OF_RANGE,
    TGA_IMAGE_TYPE_OUT_OF_RANGE,
    TGA_MISSING_COLOR_MAP,
    TGA_UNNECESSARY_COLOR_MAP,
    TGA_COLOR_MAP_FIRST_ENTRY_INDEX_INVALID,
    TGA_COLOR_MAP_FIRST_ENTRY_INDEX_OUT_OF_RANGE,
    TGA_COLOR_MAP_LENGTH_INVALID,
    TGA_COLOR_MAP_ENTRY_SIZE_INVALID,
    TGA_COLOR_MAP_ENTRY_SIZE_OUT_OF_RANGE,
    TGA_IMAGE_DESCRIPTOR_OUT_OF_RANGE,
    TGA_ID_STRING_TOO_SHORT,
    TGA_PIXEL_DEPTH_OUT_OF_RANGE,
    TGA_EXTENSION_SIZE_OUT_OF_RANGE,
    TGA_AUTHOR_NAME_NOT_NULL_TERMINATED,
    TGA_AUTHOR_COMMENT_NOT_NULL_TERMINATED,
    TGA_MONTH_OUT_OF_RANGE,
    TGA_DAY_OUT_OF_RANGE,
    TGA_HOUR_OUT_OF_RANGE,
    TGA_MINUTE_OUT_OF_RANGE,
    TGA_SECOND_OUT_OF_RANGE,
    TGA_JOB_ID_NOT_NULL_TERMINATED,
    TGA_JOB_MINUTE_OUT_OF_RANGE,
    TGA_JOB_SECOND_OUT_OF_RANGE,
    TGA_SOFTWARE_ID_NOT_NULL_TERMINATED,
    TGA_GAMMA_CORRECTION_OUT_OF_RANGE,
    TGA_EXTENSION_AREA_TOO_SHORT,
    TGA_ALPHA_ATTRIBUTES_OUT_OF_RANGE,
    TGA_ALPHA_ATTRIBUTES_CONFLICT,
    TGA_IMAGE_DATA_TOO_SHORT,
    TGA_COLOR_INDEX_OUT_OF_RANGE,
} tga_error_t;

typedef struct tga_header_tag
{
    uint8_t id_string_length;

    // 0 = no colormap. 1 = one colormap
    uint8_t color_map_type;

    tga_image_type_t image_type;
    // First element of the colormap actually used
    uint16_t color_map_first_entry_index;
    uint16_t color_map_length;
    // Bits per colormap entry: 15, 16, 24, 32
    uint8_t color_map_entry_size;
    // recommended display position in pixels between left edge of screen and left side of image
    uint16_t x_origin;
    // recommended display position in pixels between bottom edge of scfreen and bottom of image
    uint16_t y_origin;
    // width of image in pixels
    uint16_t image_width;
    // height of image in pixels
    uint16_t image_height;
    // number of bits per pixel: 8, 16, 24, 32
    uint8_t pixel_depth;
    // bits 0-3: number of alpha bits per pixel.
    // Bit 4: 0 = rows go from bottom to top. 1 = rows go from top to bottom
    // Bit 5: 0 = columns go from left to right. 1 = columns go from right to left
    uint8_t image_descriptor;
} tga_header_t;

typedef struct tga_data_tag
{
    char *id_string;
    union {
        uint8_t *color_map_data;
        uint16_t *color_map_data_u16;
        uint32_t *color_map_data_u32;
    };
    union {
        uint8_t *image_data;
        uint8_t *image_data_u8;
        uint16_t *image_data_u16;
        uint32_t *image_data_u32;
    };
} tga_data_t;

struct tga_image_tag
{
    tga_header_t header;
    tga_data_t data;
};

typedef struct tga_image_tag tga_image_t;

#define MEM0(dest)                              \
	memset((void *)(dest),0,sizeof(dest))

#define NEW0(type, count)                       \
    (type *) calloc (count, sizeof(type))

static tga_error_t
tga_parse_stream (SDL_RWops *istream, tga_image_t *t, int64_t filesize)
{
    tga_error_t error;
    size_t id_size = 0;
    size_t color_map_size = 0;
    size_t image_unpacked_size = 0;
    
    if (filesize == 0)
    {
        filesize = SDL_RWseek (istream, 0, RW_SEEK_END);
        if (filesize == -1)
        {
            error = TGA_READ_ERROR;
            goto cleanup;
        }
        if (-1 == SDL_RWseek (istream, 0, RW_SEEK_SET))
	    {
            error = TGA_READ_ERROR;
            goto cleanup;
	    }
    }
    
    MEM0 (&(t->header));
    MEM0 (&(t->data));
    
    // Field 1 - ID Length
    t->header.id_string_length = xsdl_read_uint8 (istream);
    
    // Field 2 - Color Map Type
    t->header.color_map_type = xsdl_read_uint8 (istream);
    if (!TGA_COLOR_MAP_TYPE_VALID(t))
    {
        error = TGA_COLOR_MAP_TYPE_OUT_OF_RANGE;
        goto cleanup;
    }
    
    // Field 3 - Image Type
    t->header.image_type = (tga_image_type_t) xsdl_read_uint8 (istream);
    if (!TGA_IMAGE_TYPE_VALID(t))
    {
        error = TGA_IMAGE_TYPE_OUT_OF_RANGE;
        goto cleanup;
    }
    if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_NO_COLOR_MAP
        && TGA_IMAGE_TYPE_COLOR_MAPPED(t))
    {
        error = TGA_MISSING_COLOR_MAP;
        goto cleanup;
    }
    if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP
        && !TGA_IMAGE_TYPE_COLOR_MAPPED(t))
    {
        error = TGA_UNNECESSARY_COLOR_MAP;
        goto cleanup;
    }
    
    // Field 4 - Color Map Specification
    
    // Field 4.1 - First Entry Index
    t->header.color_map_first_entry_index = SDL_ReadLE16 (istream);
    if (t->header.color_map_first_entry_index != 0
        && t->header.color_map_type == TGA_COLOR_MAP_TYPE_NO_COLOR_MAP)
    {
        error = TGA_COLOR_MAP_FIRST_ENTRY_INDEX_INVALID;
        goto cleanup;
    }
    
    // Field 4.2 - Color Map Length
    t->header.color_map_length = SDL_ReadLE16 (istream);
    if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_NO_COLOR_MAP
        && t->header.color_map_length > 0)
    {
        error = TGA_COLOR_MAP_LENGTH_INVALID;
        goto cleanup;
    }
    
    // Field 4.3 - Color Map Entry Size
    if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP
        && t->header.color_map_length == 0)
    {
        error = TGA_COLOR_MAP_LENGTH_INVALID;
        goto cleanup;
    }
    if (t->header.color_map_first_entry_index > t->header.color_map_length)
    {
        error = TGA_COLOR_MAP_FIRST_ENTRY_INDEX_OUT_OF_RANGE;
        goto cleanup;
    }
    t->header.color_map_entry_size = xsdl_read_uint8 (istream);
    if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_NO_COLOR_MAP
        && t->header.color_map_entry_size != 0)
    {
        error = TGA_COLOR_MAP_ENTRY_SIZE_INVALID;
        goto cleanup;
    }
    if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP
        && !TGA_COLOR_MAP_ENTRY_SIZE_VALID(t))
    {
        error = TGA_COLOR_MAP_ENTRY_SIZE_OUT_OF_RANGE;
        goto cleanup;
    }
    
    // 15bpp is a perfectly valid color map width, but, I don't want to
    // support it.
    if (t->header.color_map_entry_size == 15)
        t->header.color_map_entry_size = 16;
    
    // Field 5 - Image Specification
    // Field 5.1 - X Origin of Image
    t->header.x_origin = SDL_ReadLE16 (istream);

    // Field 5.2 - Y Origin of Image
    t->header.y_origin = SDL_ReadLE16 (istream);
    
    // Field 5.3 - Image Width
    t->header.image_width = SDL_ReadLE16 (istream);
    
    // Field 5.4 - Image Height
    t->header.image_height = SDL_ReadLE16 (istream);
    
    // Field 5.5 - Pixel Depth
    t->header.pixel_depth = xsdl_read_uint8 (istream);
    if (!(TGA_PIXEL_DEPTH_VALID(t) || t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP))
    {
        error = TGA_PIXEL_DEPTH_OUT_OF_RANGE;
        goto cleanup;
    }
    // 15bpp is a perfectly valid pixel depth, but, I don't want to
    // support it.
    if (t->header.pixel_depth == 15)
        t->header.pixel_depth = 16;
    
    // Field 5.6 - Image Descriptor
    t->header.image_descriptor = xsdl_read_uint8 (istream);
    if (t->header.image_descriptor & 0xC0)
    {
        error = TGA_IMAGE_DESCRIPTOR_OUT_OF_RANGE;
        goto cleanup;
    }
    
    // Field 6 - Image ID (Variable)
    id_size = t->header.id_string_length;
    if (filesize < TGA_PACKED_HEADER_LEN + id_size)
    {
        error = TGA_ID_STRING_TOO_SHORT;
        goto cleanup;
    }
    
    if (id_size == 0)
        t->data.id_string = (char *) 0;
    else
    {
        t->data.id_string = (char *) malloc (sizeof(char) * (id_size + 1));
        memset(t->data.id_string, 0, id_size + 1);
        xsdl_read_char_array (t->data.id_string, id_size, istream);
    }
    
    // Field 7 - Color Map Data (variable)
    if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP)
        color_map_size = ((size_t) t->header.color_map_length
                          * (size_t) BIT_TO_BYTE(t->header.color_map_entry_size));
    else
        color_map_size = 0;
    
    if (filesize < TGA_PACKED_HEADER_LEN + id_size + color_map_size)
    {
        error = TGA_COLOR_MAP_TOO_SHORT;
        goto cleanup;
    }
    
    if (color_map_size > 0)
    {
        t->data.color_map_data = (uint8_t *) malloc (sizeof(uint8_t) * color_map_size);
        memset(t->data.color_map_data, 0, color_map_size);
        xsdl_read_uint8_array (t->data.color_map_data, color_map_size, istream);
    }
    else
        t->data.color_map_data = (uint8_t *) 0;
    
    // Field 8 - Image Data
    if (t->header.image_type == TGA_IMAGE_TYPE_NO_IMAGE_DATA)
    {
        image_unpacked_size = 0;
        t->data.image_data = (uint8_t *) 0;
    }
    else
    {
        image_unpacked_size = TGA_IMAGE_SIZE(t) * BIT_TO_BYTE (t->header.pixel_depth);
        t->data.image_data = NEW0 (uint8_t, image_unpacked_size);
        
        // How to read the unpack the data depends on the format
        if (TGA_IMAGE_TYPE_UNCOMPRESSED(t))
        {
            // For uncompressed data, we can sanity check the file size
            if (filesize < TGA_PACKED_HEADER_LEN + id_size + color_map_size + image_unpacked_size)
                error = TGA_IMAGE_DATA_TOO_SHORT;
            xsdl_read_uint8_array (t->data.image_data, image_unpacked_size, istream);
        }
        else if (TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED(t))
        {
            int j;
            
            /* FIXME: I'm uncompressing the data here.  I should really
             * make RLE compress and RLE uncompress into methods.
             */
            
            // For RLE data, we can't sanity check the file size first.
            for (j = 0; j < t->header.image_height; j ++)
            {
                int count;
                count = xsdl_read_rle_array (&(t->data.image_data[j * t->header.image_width * BIT_TO_BYTE (t->header.pixel_depth)]),
                                             BIT_TO_BYTE (t->header.pixel_depth),
                                             t->header.image_width,
                                             istream);
                if (count == -1)
                {
                    error = TGA_IMAGE_DATA_TOO_SHORT;
                    goto cleanup;
                }
                image_unpacked_size += count;
            }
            /* Since we've uncompressed the data... */
            if (t->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED)
                t->header.image_type = TGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED;
            else if (t->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR)
                t->header.image_type = TGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR;
            else if (t->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)
                t->header.image_type = TGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE;
        }
    }

    return TGA_OK;
    
 cleanup:
    if (t->data.id_string)
        free (t->data.id_string);
    if (t->data.color_map_data)
        free (t->data.color_map_data);
    if (t->data.image_data)
        free (t->data.image_data);
    
    return error;
}

bool tga_has_image (const tga_image_t *t)
{
    if (t->header.image_type != TGA_IMAGE_TYPE_NO_IMAGE_DATA)
        return 1;
    return 0;
}

bool tga_has_palette (const tga_image_t *t)
{
    int test1, test2, test3;

    /* Some applications were known to define color maps for true
     * color images so they could use the color map area for other
     * things.  This is why the image_type is the more important
     * indicator of whether something has a color map. */
    test1 = TGA_IMAGE_TYPE_COLOR_MAPPED(t);
    test2 = t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP;
    test3 = t->header.color_map_length > 0;
    if (test1 && test2 && test3)
        return 1;
    return 0;
}

void tga_get_image_dimensions (const tga_image_t *t, unsigned int *width, unsigned int *height)
{
    if (tga_has_image (t))
    {
        *width = t->header.image_width;
        *height = t->header.image_height;
    }
    else
    {
        *width = 0;
        *height = 0;
    }
}

void tga_get_image_orientation (const tga_image_t *t, tga_hflip_t *hflip, tga_vflip_t *vflip)
{
    if (tga_has_image (t))
    {
        *hflip = t->header.image_descriptor & 0b00010000 ? RIGHT_TO_LEFT : LEFT_TO_RIGHT;
        *vflip = t->header.image_descriptor & 0b00100000 ? TOP_TO_BOTTOM : BOTTOM_TO_TOP;
    }
    else
    {
        *hflip = LEFT_TO_RIGHT;
        *vflip = BOTTOM_TO_TOP;
    }
}

unsigned int tga_get_color_map_length (const tga_image_t *t)
{
    return t->header.color_map_length;
}

unsigned int tga_get_color_map_first_index (const tga_image_t *t)
{
    return t->header.color_map_first_entry_index;
}

uint16_t *
tga_get_image_data_u16_ptr (const tga_image_t *t)
{
  return t->data.image_data_u16;
}

uint8_t *
tga_get_image_data_u8_ptr (const tga_image_t *t)
{
  return t->data.image_data_u8;
}

uint16_t *
tga_get_color_map_data_u16_ptr (const tga_image_t *t)
{
  return t->data.color_map_data_u16;
}

tga_image_t *tga_load_from_resource (const char *resource)
{
  SDL_RWops *istream;
  tga_image_t *t;
  
  t = NEW0 (tga_image_t, 1);
  istream = SDL_RWFromFile(resource, "rb");
  SDL_assert (istream != NULL);
  tga_parse_stream (istream, t, 0);
  SDL_RWclose (istream);
  return t;
}

void tga_free (tga_image_t *t)
{
  if (t->data.id_string)
    free (t->data.id_string);
  if (t->data.color_map_data)
    free (t->data.color_map_data);
  if (t->data.image_data)
    free (t->data.image_data);
  free (t);
  t = NULL;
}

