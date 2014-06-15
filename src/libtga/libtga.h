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

// Forward declarations of private structures
typedef struct tga_ctx tga_ctx_t;
typedef struct tga_image tga_image_t;

// Library context functions.  You create one per library instance.
TGA_EXPORT int
  tga_new(tga_ctx_t **ctx);
TGA_EXPORT tga_ctx_t *
  tga_ref(tga_ctx_t *ctx);
TGA_EXPORT tga_ctx_t *
  tga_unref(tga_ctx_t *ctx);
TGA_EXPORT void *
  tga_get_userdata(tga_ctx_t *ctx);
TGA_EXPORT void
  tga_set_userdata(tga_ctx_t *ctx, void *userdata);
TGA_EXPORT void
  tga_set_log_fn(tga_ctx_t *ctx,
                 void (*log_fn)(tga_ctx_t *ctx,
                                int priority, const char *file,
                                int line, const char *fn,
                                const char *format, va_list args));
TGA_EXPORT int
  tga_get_log_priority(tga_ctx_t *ctx);
TGA_EXPORT void
  tga_set_log_priority(tga_ctx_t *ctx, int priority);

// TGA Image data. One per image in memory for this context.
typedef enum tga_image_type_tag
{
  TGA_IMAGE_TYPE_NO_IMAGE_DATA = 0,
  TGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED = 1,
  TGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR = 2,
  TGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE = 3,
  TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED = 9,
  TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR = 10,
  TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE = 11
} tga_image_type_t;

typedef enum tga_error_tag {
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

typedef enum tga_hflip_tag {
  LEFT_TO_RIGHT = 0,
  RIGHT_TO_LEFT = 1
} tga_hflip_t;

typedef enum tga_vflip_tag {
  TOP_TO_BOTTOM = 1,
  BOTTOM_TO_TOP = 0
} tga_vflip_t;

TGA_EXPORT int
  tga_image_new_from_memory(tga_ctx_t *ctx, const uint8_t *mem, size_t len,
			    tga_image_t **x);
TGA_EXPORT tga_image_t *
  tga_image_ref(tga_image_t *x);
TGA_EXPORT tga_image_t *
  tga_image_unref(tga_image_t *x);
TGA_EXPORT tga_ctx_t *
  tga_image_get_ctx(tga_image_t *x);
TGA_EXPORT bool
  tga_has_image (const tga_image_t *t);
TGA_EXPORT bool
  tga_has_palette (const tga_image_t *t);
TGA_EXPORT void
  tga_get_image_dimensions (const tga_image_t *t,
                            unsigned int *width, unsigned int *height);

// Get the flags from the Tga file that indicate that it is flipped
// horizontally or vertically
TGA_EXPORT void
  tga_get_image_orientation (const tga_image_t *t, tga_hflip_t *hflip,
			     tga_vflip_t *vflip);
#if 0
TGA_LOCAL color_format_t
  tga_get_image_color_format (const tga_image_t *t);
#endif

#if 0
TGA_EXPORT tga_color_format_t
  tga_get_palette_color_format (const tga_image_t *t);
TGA_EXPORT unsigned int
  tga_get_color_map_first_index (const tga_image_t *t);
TGA_EXPORT unsigned int
  tga_get_color_map_size (const tga_image_t *t);
#endif
#endif
