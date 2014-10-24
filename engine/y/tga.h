#ifndef TGA_H_INCLUDED
#define TGA_H_INCLUDED

#include <glib.h>
#include <gio/gio.h>

#define TARGA_PACKED_HEADER_LEN (18)

#define TARGA_COLOR_MAP_TYPE_NO_COLOR_MAP (0)
#define TARGA_COLOR_MAP_TYPE_COLOR_MAP (1)

#define TARGA_COLOR_MAP_TYPE_VALID(T)							   \
	(T->header.color_map_type == TARGA_COLOR_MAP_TYPE_NO_COLOR_MAP	\
	 || T->header.color_map_type == TARGA_COLOR_MAP_TYPE_COLOR_MAP)

#define BIT_TO_BYTE(n) (((n)+7)/8)

typedef enum targa_image_type_tag
{
    TARGA_IMAGE_TYPE_NO_IMAGE_DATA = 0,
    TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED = 1,
    TARGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR = 2,
    TARGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE = 3,
    TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED = 9,
    TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR = 10,
    TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE = 11
} targa_image_type_t;

#define TARGA_IMAGE_TYPE_VALID(T)										\
	(T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED	\
	 || T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR \
	 || T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE \
	 || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED \
	 || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR \
	 || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)

#define TARGA_IMAGE_TYPE_COLOR_MAPPED(T)								\
	(T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED	\
	 || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED)

#define TARGA_IMAGE_TYPE_TRUE_COLOR(T)									\
	(T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR	\
	 || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR)

#define TARGA_IMAGE_TYPE_BLACK_AND_WHITE(T)								\
	(T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE \
	 || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)

#define TARGA_IMAGE_TYPE_UNCOMPRESSED(T) \
	(T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED	\
	 || T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR \
	 || T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE)

#define TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED(T)							\
	(T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED \
	 || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR \
	 || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)

#define TARGA_COLOR_SIZE_15_BITS (15)
#define TARGA_COLOR_SIZE_16_BITS (16)
#define TARGA_COLOR_SIZE_24_BITS (24)
#define TARGA_COLOR_SIZE_32_BITS (32)

#define TARGA_COLOR_MAP_ENTRY_SIZE_VALID(T)						  \
	((T->header.color_map_entry_size == TARGA_COLOR_SIZE_15_BITS)	 \
	 || (T->header.color_map_entry_size == TARGA_COLOR_SIZE_16_BITS) \
	 || (T->header.color_map_entry_size == TARGA_COLOR_SIZE_24_BITS)	\
	 || (T->header.color_map_entry_size == TARGA_COLOR_SIZE_32_BITS))

#define TARGA_PIXEL_DEPTH_VALID(T)									 \
	((T->header.pixel_depth == TARGA_COLOR_SIZE_15_BITS)	 \
	 || (T->header.pixel_depth == TARGA_COLOR_SIZE_16_BITS) \
	 || (T->header.pixel_depth == TARGA_COLOR_SIZE_24_BITS)	\
	 || (T->header.pixel_depth == TARGA_COLOR_SIZE_32_BITS))

#define TARGA_EXTENSION_SIZE (495)

#define TARGA_ALPHA_ATTRIBUTES_NO_ALPHA (0)
#define TARGA_ALPHA_ATTRIBUTES_UNDEFINED_IGNORED (1)
#define TARGA_ALPHA_ATTRIBUTES_UNDEFINED_RETAINED (2)
#define TARGA_ALPHA_ATTRIBUTES_ALPHA (3)
#define TARGA_ALPHA_ATTRIBUTES_ALPHA_PREMULTIPLIED (4)

#define TARGA_ALPHA_ATTRIBUTES_VALID(T) \
	((T->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_NO_ALPHA)	\
	 || (T->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_UNDEFINED_IGNORED) \
	 || (T->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_UNDEFINED_RETAINED) \
	 || (T->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_ALPHA)	\
	 || (T->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_ALPHA_PREMULTIPLIED))

#define TARGA_IMAGE_SIZE(T)											\
	((gsize) T->header.image_width * (gsize) T->header.image_height)

typedef enum targa_error_tag
{
    // Generic OK
    TARGA_OK = 0,
    // Generic ERROR
    TARGA_ERROR = 1,
    // Parsing error list begins here
    TARGA_HEADER_TOO_SHORT,
    TARGA_READ_ERROR,
    TARGA_OUT_OF_MEMORY,
    TARGA_COLOR_MAP_TOO_SHORT,
    TARGA_COLOR_MAP_TYPE_OUT_OF_RANGE,
    TARGA_IMAGE_TYPE_OUT_OF_RANGE,
    TARGA_MISSING_COLOR_MAP,
    TARGA_UNNECESSARY_COLOR_MAP,
    TARGA_COLOR_MAP_FIRST_ENTRY_INDEX_INVALID,
    TARGA_COLOR_MAP_FIRST_ENTRY_INDEX_OUT_OF_RANGE,
    TARGA_COLOR_MAP_LENGTH_INVALID,
    TARGA_COLOR_MAP_ENTRY_SIZE_INVALID,
    TARGA_COLOR_MAP_ENTRY_SIZE_OUT_OF_RANGE,
    TARGA_IMAGE_DESCRIPTOR_OUT_OF_RANGE,
    TARGA_ID_STRING_TOO_SHORT,
    TARGA_PIXEL_DEPTH_OUT_OF_RANGE,
    TARGA_EXTENSION_SIZE_OUT_OF_RANGE,
    TARGA_AUTHOR_NAME_NOT_NULL_TERMINATED,
    TARGA_AUTHOR_COMMENT_NOT_NULL_TERMINATED,
    TARGA_MONTH_OUT_OF_RANGE,
    TARGA_DAY_OUT_OF_RANGE,
    TARGA_HOUR_OUT_OF_RANGE,
    TARGA_MINUTE_OUT_OF_RANGE,
    TARGA_SECOND_OUT_OF_RANGE,
    TARGA_JOB_ID_NOT_NULL_TERMINATED,
    TARGA_JOB_MINUTE_OUT_OF_RANGE,
    TARGA_JOB_SECOND_OUT_OF_RANGE,
    TARGA_SOFTWARE_ID_NOT_NULL_TERMINATED,
    TARGA_GAMMA_CORRECTION_OUT_OF_RANGE,
    TARGA_EXTENSION_AREA_TOO_SHORT,
    TARGA_ALPHA_ATTRIBUTES_OUT_OF_RANGE,
    TARGA_ALPHA_ATTRIBUTES_CONFLICT,
    TARGA_IMAGE_DATA_TOO_SHORT,
    TARGA_COLOR_INDEX_OUT_OF_RANGE,
} targa_error_t;

typedef enum targa_hflip_tag
{
    LEFT_TO_RIGHT = 0,
    RIGHT_TO_LEFT = 1
} targa_hflip_t;

typedef enum targa_vflip_tag
{
    TOP_TO_BOTTOM = 1,
    BOTTOM_TO_TOP = 0
} targa_vflip_t;

typedef struct targa_header_tag
{
    guint8 id_string_length;

    // 0 = no colormap. 1 = one colormap
    guint8 color_map_type;

    targa_image_type_t image_type;
    // First element of the colormap actually used
    guint16 color_map_first_entry_index;
    guint16 color_map_length;
    // Bits per colormap entry: 15, 16, 24, 32
    guint8 color_map_entry_size;
    // recommended display position in pixels between left edge of screen and left side of image
    guint16 x_origin;
    // recommended display position in pixels between bottom edge of scfreen and bottom of image
    guint16 y_origin;
    // width of image in pixels
    guint16 image_width;
    // height of image in pixels
    guint16 image_height;
    // number of bits per pixel: 8, 16, 24, 32
    guint8 pixel_depth;
    // bits 0-3: number of alpha bits per pixel.
    // Bit 4: 0 = rows go from bottom to top. 1 = rows go from top to bottom
    // Bit 5: 0 = columns go from left to right. 1 = columns go from right to left
    guint8 image_descriptor;
} targa_header_t;

typedef struct targa_data_tag
{
    char *id_string;
  union {
    guint8 *color_map_data;
    guint16 *color_map_data_u16;
    guint32 *color_map_data_u32;
  };
    union {
      guint8 *image_data;
      guint8 *image_data_u8;
      guint16 *image_data_u16;
      guint32 *image_data_u32;
    };
} targa_data_t;

typedef struct targa_image_tag
{
    targa_header_t header;
    targa_data_t data;
} targa_image_t;

targa_image_t *tga_load(const char *name);
targa_image_t *tga_load_from_resource (const gchar *resource);
void tga_free (targa_image_t *t);

// Reads in the file FP and unpacks all the information into a targa image
targa_error_t targa_parse_stream (GInputStream *fp, targa_image_t *t, gsize filesize);

gboolean targa_has_image (const targa_image_t *t);

gboolean targa_has_palette (const targa_image_t *t);

void targa_get_image_dimensions (const targa_image_t *t, guint *width, guint *height);

// Get the flags from the Targa file that indicate that it is flipped horizontally or vertically
void targa_get_image_orientation (const targa_image_t *t,
                                  targa_hflip_t *hflip,
                                  targa_vflip_t *vflip);

guint targa_get_color_map_first_index (const targa_image_t *t);

guint targa_get_color_map_length (const targa_image_t *t);

guint16 *tga_get_image_data_u16_ptr (const targa_image_t *t);
guint8 *tga_get_image_data_u8_ptr (const targa_image_t *t);
guint16 *tga_get_color_map_data_u16_ptr (const targa_image_t *t);

#endif // TGA_H_INCLUDED
