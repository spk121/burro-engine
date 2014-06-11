#pragma once

#define BIT_TO_BYTE(bits) (((size_t) bits + 7) >> 3)


/* bit 0 - invalid
 * bit 1 - index
 * bit 2 - gray
 * bit 3 - alpha
 * bit 4 - overlay
 * bit 5 - rgb
 * bit 6 - bgr */
#define COLOR_TYPE_INVALID 1
#define COLOR_TYPE_INDEX   (1 << 1)
#define COLOR_TYPE_GRAY    (1 << 2)
#define COLOR_TYPE_ALPHA   (1 << 3)
#define COLOR_TYPE_OVERLAY (1 << 4)
#define COLOR_TYPE_RGB     (1 << 5)
#define COLOR_TYPE_BGR     (1 << 6)

#define COLOR_TYPE_OBGR	    (COLOR_TYPE_OVERLAY | COLOR_TYPE_BGR)
#define COLOR_TYPE_ABGR	    (COLOR_TYPE_ALPHA | COLOR_TYPE_BGR)
#define COLOR_TYPE_ORGB	    (COLOR_TYPE_OVERLAY | COLOR_TYPE_RGB)
#define COLOR_TYPE_ARGB	    (COLOR_TYPE_ALPHA | COLOR_TYPE_RGB)

#define COLOR_FORMAT(bpp,type,alpha,overlay,rgb)	\
  (((bpp) << 20)					\
   | ((type) << 12)					\
   | ((alpha) << 8)					\
   | ((overlay) << 4)					\
   | (rgb))

#define COLOR_FORMAT_BPP(f)	((f) >> 20)
#define COLOR_FORMAT_TYPE(f) (((f) >> 12) & 0xff)
#define COLOR_FORMAT_ALPHA(f)	(((f) >> 8) & 0x0f)
#define COLOR_FORMAT_OVERLAY(f)	(((f) >>  4) & 0x0f)
#define COLOR_FORMAT_RGB(f)	((f) & 0x0f)

#define COLOR_FORMAT_TYPE_IS_BGR(f)		\
  (COLOR_FORMAT_TYPE(f) & COLOR_TYPE_BGR)
#define COLOR_FORMAT_TYPE_IS_RGB(f)		\
  (COLOR_FORMAT_TYPE(f) & COLOR_TYPE_RGB)
#define COLOR_FORMAT_TYPE_IS_ALPHA(f)		\
  (COLOR_FORMAT_TYPE(f) & COLOR_TYPE_ALPHA)
#define COLOR_FORMAT_TYPE_IS_OVERLAY(f)		\
  (COLOR_FORMAT_TYPE(f) & COLOR_TYPE_OVERLAY)

#define CONVERT_8888_TO_0565(s)			\
  ((((s) >> 3) & 0x001f) |			\
   (((s) >> 5) & 0x07e0) |			\
   (((s) >> 8) & 0xf800))

#define CONVERT_0565_TO_0888(s)				\
  (((((s) << 3) & 0xf8) | (((s) >> 2) & 0x7)) |		\
   ((((s) << 5) & 0xfc00) | (((s) >> 1) & 0x300)) |	\
   ((((s) << 8) & 0xf80000) | (((s) << 3) & 0x70000)))

#define CONVERT_0565_TO_8888(s) (CONVERT_0565_TO_0888(s) | 0xff000000)

/* Trivial versions that are useful in macros */
#define CONVERT_8888_TO_8888(s) (s)
#define CONVERT_x888_TO_8888(s) ((s) | 0xff000000)
#define CONVERT_0565_TO_0565(s) (s)

#define PACK16(o,c1,c2,c3) ((c3) | ((c2) << 5) | ((c1) << 10) | ((o) << 15))
#define PACK24(c1,c2,c3) ((c3) | ((c2) << 8) | ((c1) << 16))
#define PACK32(a,c1,c2,c3) ((c3) | ((c2) << 8) | ((c1) << 16) | ((a) << 24))
#define PTR_PACK16(p,o,c1,c2,c3)				\
  do {								\
    guint8 *__tmp = (p);					\
    __tmp[0] = ((c3) | (c2) << 5);				\
    __tmp[1] = (((c2) >> 3) | ((c1) << 2) | ((o) << 7));	\
  } while (0)
#define PTR_PACK24(p,c1,c2,c3)			\
  do {						\
    guint8 *__tmp = (p);			\
    __tmp[0] = (c3);				\
    __tmp[1] = (c2);				\
    __tmp[2] = (c1);				\
  } while (0)
#define PTR_PACK32(p,a,c1,c2,c3)		\
  do {						\
    guint8 *__tmp = (p);			\
    __tmp[0] = (c3);				\
    __tmp[1] = (c2);				\
    __tmp[2] = (c1);				\
    __tmp[3] = (a);				\
  } while (0)
#define PTR_SWAPPACK32(p,a,c1,c2,c3)		\
  do {						\
    guint8 *__tmp = (p);			\
    __tmp[0] = (a);				\
    __tmp[1] = (c1);				\
    __tmp[2] = (c2);				\
    __tmp[3] = (c3);				\
  } while (0)
#define PTR_UNPACK16(p,o,c1,c2,c3)		\
  do {						\
    guint16 __tmp = *((guint16 *)(p));		\
    (c3) = __tmp & 0b0000000000011111;		\
    (c2) = (__tmp & 0b0000001111100000) >> 5;	\
    (c1) = (__tmp & 0b0111110000000000) >> 10;	\
    (o) = __tmp >> 15;				\
  } while (0)
#define PTR_UNPACK24(p,c1,c2,c3)		\
  do {						\
    guint8 *__tmp = (p);			\
    (c3) = __tmp[0];				\
    (c2) = __tmp[1];				\
    (c1) = __tmp[2];				\
  } while (0)
#define PTR_UNPACK32(p,a,c1,c2,c3)		\
  do {						\
    guint8 *__tmp = (p);			\
    (c3) = __tmp[0];				\
    (c2) = __tmp[1];				\
    (c1) = __tmp[2];				\
    (a) = __tmp[3];				\
  } while (0)


typedef enum color_format_tag
  {
    /* invalid */
    COLOR_x = COLOR_FORMAT(0, COLOR_TYPE_INVALID, 0, 0, 0),

    /* Colors used with Cairo */
    COLOR_a8r8g8b8 = COLOR_FORMAT(32, COLOR_TYPE_ARGB, 8, 0, 8),

    /* Colors used by the FauxDS engine for in-memory storage */
    COLOR_i8 = COLOR_FORMAT(8, COLOR_TYPE_INDEX, 0, 0, 0),
    COLOR_a1r8g8b8 = COLOR_FORMAT(32, COLOR_TYPE_ARGB, 1, 0, 5),

    /* Colors stored in Targa files that we may load */
    /* Targa 8-bit */
    // COLOR_i8;
    COLOR_g8 = COLOR_FORMAT(8, COLOR_TYPE_GRAY, 0, 0, 0),
    /* Targa 16-bit */
    COLOR_i16 = COLOR_FORMAT(16, COLOR_TYPE_INDEX, 0, 0, 0),
    COLOR_g16 = COLOR_FORMAT(16, COLOR_TYPE_GRAY, 0, 0, 0),
    COLOR_x1r5g5b5 = COLOR_FORMAT(16, COLOR_TYPE_RGB, 0, 0, 5),
    COLOR_o1r5g5b5 = COLOR_FORMAT(16, COLOR_TYPE_ORGB, 0, 1, 5),
    COLOR_o1b5g5r5 = COLOR_FORMAT(16, COLOR_TYPE_OBGR, 0, 1, 5),

    /* Targa's 24 bit */
    COLOR_b8g8r8 = COLOR_FORMAT(24, COLOR_TYPE_BGR, 0, 0, 8),
    COLOR_r8g8b8 = COLOR_FORMAT(24, COLOR_TYPE_RGB, 0, 0, 8),

    /* Targa's 32 bit */
    COLOR_x8b8g8r8 = COLOR_FORMAT(32, COLOR_TYPE_BGR, 0, 0, 8),
    COLOR_x8r8g8b8 = COLOR_FORMAT(32, COLOR_TYPE_RGB, 0, 0, 8),
    COLOR_a8b8g8r8 = COLOR_FORMAT(32, COLOR_TYPE_ABGR, 8, 0, 8),

  } color_format_t;

#define KEY_COLOR_16BIT PACK16(0,31,0,31)
#define KEY_COLOR_24BIT PACK24(255,0,255)
#define KEY_COLOR_32BIT PACK32(0

guint32 convert_color (guint32 c, color_format_t input, color_format_t output);
void convert_color_array (guint8 *out_arr,
			  guint8 *in_arr, 
			  guint32 count,
			  color_format_t in_cf,
			  color_format_t out_cf);
void colorval_to_rgba (guint8 *c, color_format_t cf, guint *r, guint *g, guint *b, guint *a);
void rgba_to_colorval (guint8 *c, color_format_t cf, guint r, guint g, guint b, guint a);

