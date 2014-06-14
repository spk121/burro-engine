#ifndef LIBTGA_PRIVATE_H
#define LIBTGA_PRIVATE_H

#include "libtga.h"

#define LOG_EMERG (0)
#define LOG_ALERT (1)
#define LOG_CRIT (2)
#define LOG_ERR (3)
#define LOG_WARNING (4)
#define LOG_NOTICE (5)
#define LOG_INFO (6)
#define LOG_DEBUG (7)

static inline void __attribute__((always_inline, format(printf, 2, 3)))
  tga_log_null(struct tga_ctx *ctx, const char *format, ...) {}

#define tga_log_cond(ctx, prio, arg...) \
  do { \
    if (tga_get_log_priority(ctx) >= prio) \
      tga_log(ctx, prio, __FILE__, __LINE__, __FUNCTION__, ## arg); \
  } while (0)

#ifdef ENABLE_LOGGING
#  ifdef ENABLE_DEBUG
#    define dbg(ctx, arg...) tga_log_cond(ctx, LOG_DEBUG, ## arg)
#  else
#    define dbg(ctx, arg...) tga_log_null(ctx, ## arg)
#  endif
#  define info(ctx, arg...) tga_log_cond(ctx, LOG_INFO, ## arg)
#  define err(ctx, arg...) tga_log_cond(ctx, LOG_ERR, ## arg)
#else
#  define dbg(ctx, arg...) tga_log_null(ctx, ## arg)
#  define info(ctx, arg...) tga_log_null(ctx, ## arg)
#  define err(ctx, arg...) tga_log_null(ctx, ## arg)
#endif

#ifndef HAVE_SECURE_GETENV
#  ifdef HAVE___SECURE_GETENV
#    define secure_getenv __secure_getenv
#  else
#    error neither secure_getenv nor __secure_getenv is available
#  endif
#endif

void
  tga_log(struct tga_ctx *ctx,
          int priority, const char *file, int line, const char *fn,
          const char *format, ...)
    __attribute__((format(printf, 6, 7)));

#define TARGA_PACKED_HEADER_LEN (18)
#define TARGA_PACKED_FOOTER_LEN (26)
#define TARGA_PACKED_FOOTER_STRING_START (8)
#define TARGA_PACKED_FOOTER_STRING_LEN (16)
#define TARGA_ID_STRING_MAX_LEN (UINT8_MAX)
#define TARGA_AUTHOR_NAME_LEN (41)
#define TARGA_AUTHOR_COMMENT_LINES_NUM (4)
#define TARGA_AUTHOR_COMMENT_LINE_LEN (81)
#define TARGA_JOB_ID_LEN (41)
#define TARGA_SOFTWARE_ID_LEN (41)
#define TARGA_SIGNATURE_LEN (18)
#define TARGA_COLOR_CORRECTION_TABLE_LEN (1024)
#define TARGA_EXTENSION_AREA_LEN (495)
#define TARGA_MAGIC_SIGNATURE_STRING "TRUEVISION-XFILE."

#define TARGA_COLOR_MAP_TYPE_NO_COLOR_MAP (0)
#define TARGA_COLOR_MAP_TYPE_COLOR_MAP (1)

#define TARGA_COLOR_MAP_TYPE_VALID(T)                                   \
  (T->header.color_map_type == TARGA_COLOR_MAP_TYPE_NO_COLOR_MAP        \
   || T->header.color_map_type == TARGA_COLOR_MAP_TYPE_COLOR_MAP)

#define TARGA_IMAGE_TYPE_NO_IMAGE 0
#

#define TARGA_IMAGE_TYPE_VALID(T)                                       \
  (T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED   \
   || T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR  \
   || T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE \
   || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED \
   || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR \
   || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)

#define TARGA_IMAGE_TYPE_COLOR_MAPPED(T)                                \
  (T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED   \
   || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED)

#define TARGA_IMAGE_TYPE_TRUE_COLOR(T)                                  \
  (T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR     \
   || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR)

#define TARGA_IMAGE_TYPE_BLACK_AND_WHITE(T)                             \
  (T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE \
   || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)

#define TARGA_IMAGE_TYPE_UNCOMPRESSED(T)                                \
  (T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED   \
   || T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR  \
   || T->header.image_type == TARGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE)

#define TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED(T)                          \
  (T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED \
   || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR \
   || T->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)

#define TARGA_COLOR_SIZE_15_BITS (15)
#define TARGA_COLOR_SIZE_16_BITS (16)
#define TARGA_COLOR_SIZE_24_BITS (24)
#define TARGA_COLOR_SIZE_32_BITS (32)

#define TARGA_COLOR_MAP_ENTRY_SIZE_VALID(T)                             \
  ((T->header.color_map_entry_size == TARGA_COLOR_SIZE_15_BITS)         \
   || (T->header.color_map_entry_size == TARGA_COLOR_SIZE_16_BITS)      \
   || (T->header.color_map_entry_size == TARGA_COLOR_SIZE_24_BITS)      \
   || (T->header.color_map_entry_size == TARGA_COLOR_SIZE_32_BITS))

#define TARGA_PIXEL_DEPTH_8_BITS (8)  // For 8-bit greyscale
#define TARGA_PIXEL_DEPTH_15_BITS (15)
#define TARGA_PIXEL_DEPTH_16_BITS (16)
#define TARGA_PIXEL_DEPTH_24_BITS (24)
#define TARGA_PIXEL_DEPTH_32_BITS (32)

#define TARGA_PIXEL_DEPTH_VALID(T)                               \
  ((T->header.pixel_depth == TARGA_PIXEL_DEPTH_8_BITS)           \
   || (T->header.pixel_depth == TARGA_PIXEL_DEPTH_15_BITS)       \
   || (T->header.pixel_depth == TARGA_PIXEL_DEPTH_16_BITS)       \
   || (T->header.pixel_depth == TARGA_PIXEL_DEPTH_24_BITS)       \
   || (T->header.pixel_depth == TARGA_PIXEL_DEPTH_32_BITS))

#define TARGA_EXTENSION_SIZE (495)

#define TARGA_ALPHA_ATTRIBUTES_NO_ALPHA (0)
#define TARGA_ALPHA_ATTRIBUTES_UNDEFINED_IGNORED (1)
#define TARGA_ALPHA_ATTRIBUTES_UNDEFINED_RETAINED (2)
#define TARGA_ALPHA_ATTRIBUTES_ALPHA (3)
#define TARGA_ALPHA_ATTRIBUTES_ALPHA_PREMULTIPLIED (4)

#define TARGA_ALPHA_ATTRIBUTES_VALID(T) \
        ((T->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_NO_ALPHA)      \
         || (T->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_UNDEFINED_IGNORED) \
         || (T->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_UNDEFINED_RETAINED) \
         || (T->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_ALPHA)      \
         || (T->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_ALPHA_PREMULTIPLIED))

#define TARGA_IMAGE_SIZE(T)                                             \
  ((size_t) T->header.image_width * (size_t) T->header.image_height)

typedef struct targa_header_tag
{
        uint8_t id_string_length;

        // 0 = no colormap. 1 = one colormap
        uint8_t color_map_type;

        targa_image_type_t image_type;
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
} targa_header_t;

typedef struct targa_extension_tag
{
        uint16_t extension_size;
        char author_name[TARGA_AUTHOR_NAME_LEN];
        char author_comment[TARGA_AUTHOR_COMMENT_LINES_NUM][TARGA_AUTHOR_COMMENT_LINE_LEN];
        uint16_t month;
        uint16_t day;
        uint16_t year;
        uint16_t hour;
        uint16_t minute;
        uint16_t second;
        char job_id[TARGA_JOB_ID_LEN];
        uint16_t job_hour;
        uint16_t job_minute;
        uint16_t job_second;
        char software_id[TARGA_SOFTWARE_ID_LEN];
        uint16_t version_number;
        char version_letter;
        uint32_t key_color_argb;
        uint16_t pixel_aspect_ratio_numerator;
        uint16_t pixel_aspect_ratio_denominator;
        uint16_t gamma_correction_factor_numerator;
        uint16_t gamma_correction_factor_denominator;
        uint32_t color_correction_offset;
        uint32_t stamp_offset;
        uint32_t scan_line_offset;
        uint8_t alpha_attribute;
        uint8_t stamp_width;
        uint8_t stamp_height;
} targa_extension_t;

typedef struct targa_footer_tag
{
        uint32_t extension_offset;
        uint32_t developer_area_offset;
        char signature[TARGA_SIGNATURE_LEN];
} targa_footer_t;

typedef struct targa_data_tag
{
        char *id_string;
        uint8_t *color_map_data;
        uint8_t *image_data;
        uint16_t *scan_line_table;
        uint8_t *postage_stamp;
        uint16_t *color_correction_table;

} targa_data_t;

typedef struct targa_image_tag
{
        unsigned int version;
        unsigned int extended_info;
        targa_header_t header;
        targa_data_t data;
        targa_extension_t extension;
        targa_footer_t footer;
} targa_image_t;

typedef struct tga_image_ctx {
  struct tga_ctx *ctx;
  int refcount;
  targa_image_t *image;
} tga_image_ctx_t;


#endif
