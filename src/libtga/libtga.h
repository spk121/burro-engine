#ifndef LIBTGA_H
#define LIBTGA_H

#include <stdarg.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#if defined _WIN32 || defined __CYGWIN__
  #ifdef BUILDING_DLL
    #ifdef __GNUC__
      #define TGA_EXPORT __attribute__ ((dllexport)
    #else
      #define TGA_EXPORT __declspec(dllexport)
    #endif
  #else
    #ifdef __GNUC__
      #define TGA_EXPORT __attribute__ ((dllimport))
    #else
      #define TGA_EXPORT __declspec(dllimport)
    #endif
  #endif
  #define TGA_LOCAL
#else
  #if __GNUC__ >= 4
    #define TGA_EXPORT __attribute__ ((visibility ("default")))
    #define TGA_LOCAL __attribute__ ((visibility ("hidden")))
  #else
    #define TGA_EXPORT
    #define TGA_LOCAL
  #endif
#endif

typedef struct tga_ctx tga_ctx_t;
typedef struct tga_image_ctx tga_image_ctx_t;

TGA_EXPORT void *
  tga_get_userdata(struct tga_ctx *ctx);
TGA_EXPORT void
  tga_set_userdata(struct tga_ctx *ctx, void *userdata);
TGA_EXPORT int
  tga_new(struct tga_ctx **ctx);
TGA_EXPORT struct
  tga_ctx *tga_ref(struct tga_ctx *ctx);
TGA_EXPORT struct
  tga_ctx *tga_unref(struct tga_ctx *ctx);
TGA_EXPORT void
  tga_set_log_fn(struct tga_ctx *ctx,
                 void (*log_fn)(struct tga_ctx *ctx,
                                int priority, const char *file,
                                int line, const char *fn,
                                const char *format, va_list args));
TGA_EXPORT int
  tga_get_log_priority(struct tga_ctx *ctx);
TGA_EXPORT void
  tga_set_log_priority(struct tga_ctx *ctx, int priority);
TGA_EXPORT tga_image_ctx_t *
  tga_image_ref(tga_image_ctx_t *x);
TGA_EXPORT tga_image_ctx_t *
  tga_image_unref(tga_image_ctx_t *x);
TGA_EXPORT struct tga_ctx *
  tga_image_get_ctx(tga_image_ctx_t *x);
TGA_EXPORT int
  tga_image_new_from_memory(struct tga_ctx *ctx, const uint8_t *mem,
                            size_t len,
                            tga_image_ctx_t **x);


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

typedef enum targa_error_tag {
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

typedef enum targa_hflip_tag {
  LEFT_TO_RIGHT = 0,
  RIGHT_TO_LEFT = 1
} targa_hflip_t;

typedef enum targa_vflip_tag {
  TOP_TO_BOTTOM = 1,
  BOTTOM_TO_TOP = 0
} targa_vflip_t;


// Reads in the file FP and unpacks all the information into a targa image
// targa_error_t targa_parse_stream (GInputStream *fp, targa_image_t *t);

TGA_EXPORT bool
  tga_has_image (const tga_image_ctx_t *t);
TGA_EXPORT bool
  tga_has_palette (const tga_image_ctx_t *t);
TGA_EXPORT void
  tga_get_image_dimensions (const tga_image_ctx_t *t,
                            unsigned int *width, unsigned int *height);

// Get the flags from the Targa file that indicate that it is flipped
// horizontally or vertically
TGA_EXPORT void
  targa_get_image_orientation (const tga_image_ctx_t *t,
                               targa_hflip_t *hflip,
                               targa_vflip_t *vflip);
#if 0
TGA_EXPORT tga_color_format_t
  tga_get_image_color_format (const tga_image_ctx_t *t);
TGA_EXPORT tga_color_format_t
  tga_get_palette_color_format (const tga_image_ctx_t *t);
TGA_EXPORT unsigned int
  tga_get_color_map_first_index (const tga_image_ctx_t *t);
TGA_EXPORT unsigned int
  targa_get_color_map_size (const tga_image_ctx_t *t);
#endif
#endif
