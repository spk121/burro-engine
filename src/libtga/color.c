#include <stdbool.h>
#include <stdint.h>
#include "color.h"
#include "libtga-private.h"

static uint32_t
convert32 (uint32_t c, color_format_t input, color_format_t output);
static uint32_t
convert16 (uint32_t c, color_format_t input, color_format_t output, bool key);
static uint32_t
upconvert_16_32 (uint32_t c, color_format_t input, color_format_t output);
static uint32_t
downconvert_32_16 (uint32_t c, color_format_t input, color_format_t output);

uint32_t
convert_color (uint32_t c, color_format_t input, color_format_t output)
{
  unsigned int bpp_in = COLOR_FORMAT_BPP (input);
  unsigned int bpp_out = COLOR_FORMAT_BPP (output);

  if (input == output)
    return c;
  if (bpp_in >= 24 && bpp_out >= 24)
    return convert32 (c, input, output);
  if (bpp_in == 16 && bpp_out == 16)
    return convert16 (c, input, output, false);
  else if (bpp_in == 16 && bpp_out >= 24)
    return upconvert_16_32 (c, input, output);
  else if (bpp_in >= 24 && bpp_out == 16)
    return downconvert_32_16 (c, input, output);

  return 0;
}

void
convert_color_array (uint8_t *out_arr, uint8_t *in_arr, uint32_t count,
                     color_format_t in_cf, color_format_t out_cf)
{
  unsigned int i;
  int in_bpp, out_bpp;
  uint32_t in_c, out_c;

  in_bpp = COLOR_FORMAT_BPP (in_cf);
  out_bpp = COLOR_FORMAT_BPP (out_cf);

  for (i = 0; i < count; i++) {
    switch (BIT_TO_BYTE (in_bpp)) {
    case 2:
      in_c = (in_arr[i * 2] + (in_arr[i * 2 + 1] << 8));
      break;
    case 3:
      in_c = (in_arr[i * 3] + (in_arr[i * 3 + 1] << 8)
              + (in_arr[i * 3 + 2] << 16));
      break;
    case 4:
      in_c = (in_arr[i * 4] + (in_arr[i * 4 + 1] << 8)
              + (in_arr[i * 4 + 2] << 16) + (in_arr[i * 4 + 3] << 24));
      break;
    }
    out_c = convert_color (in_c, in_cf, out_cf);
    switch (BIT_TO_BYTE (out_bpp)) {
    case 2:
      out_arr[2 * i] = out_c & 0x00ff;
      out_arr[2 * i + 1] = (out_c & 0xff00) >> 8;
      break;
    case 3:
      out_arr[3 * i] = out_c & 0x0000ff;
      out_arr[3 * i + 1] = (out_c & 0x00ff00) >> 8;
      out_arr[3 * i + 2] = (out_c & 0xff0000) >> 16;
      break;
    case 4:
      out_arr[4 * i] = out_c & 0x000000ff;
      out_arr[4 * i + 1] = (out_c & 0x0000ff00) >> 8;
      out_arr[4 * i + 2] = (out_c & 0x00ff0000) >> 16;
      out_arr[4 * i + 3] = (out_c & 0xff000000) >> 24;
      break;
    }
  }
}

static uint32_t
convert32 (uint32_t c, color_format_t input, color_format_t output)
{
  if (!COLOR_FORMAT_TYPE_IS_ALPHA (input)
      && COLOR_FORMAT_TYPE_IS_ALPHA (output)) {
    /* Add 0xFF for fully opaque alpha.  */
    c = c | 0xff000000;
  }
  if (COLOR_FORMAT_TYPE_IS_ALPHA (input)
      && !COLOR_FORMAT_TYPE_IS_ALPHA (output)) {
    /* Strip alpha.  */
    c = c & 0x00ffffff;
    /* FIXME: turn fully transparent into a key color? */
  }
  if (COLOR_FORMAT_TYPE_IS_RGB (input) != COLOR_FORMAT_TYPE_IS_RGB (output)) {
    /* Swap r and b */
    uint32_t c2 = ((c & 0xff00ff00) | (c & 0x00ff0000) >> 16
                   | (c & 0x000000ff) << 16);
    c = c2;
  }
  return c;
}

static uint32_t
convert16 (uint32_t c, color_format_t input, color_format_t output, bool key)
{
  const uint32_t rgb_mask = 0b0111111111111111;
  const uint32_t o_mask = 0b1000000000000000;
  const uint32_t a_mask = 0b1000000000000000;
  const uint32_t orgb_mask = 0b1111111111111111;
  const uint32_t argb_mask = 0b1111111111111111;
  const uint32_t argb_transparent = 0b0000000000000000;
  const uint32_t orgb_transparent = 0b1000000000000000;
  /*
    OUTPUT        | OUTPUT ARGB    | OUTPUT ORGB       | OUTPUT xRGB
    ---------------------------------------------------------------
    INPUT ARGB    | keep alpha     | swap alpha        | low alpha -> key
    INPUT ORGB    | swap overlay   | keep overlay      | high overlay -> key
    INPUT xRGB    | key->low alpha | key->high overlay | keep
  */
  if (COLOR_FORMAT_TYPE_IS_ALPHA (input)) {
    if (COLOR_FORMAT_TYPE_IS_OVERLAY (output)) {
      /* Swap the topbit. */
      c = c ^ a_mask;
    }
    else if (!COLOR_FORMAT_TYPE_IS_ALPHA (output)
             && !COLOR_FORMAT_TYPE_IS_OVERLAY (output)) {
      /* Set to key color when alpha is low.  */
      if (key && (c & a_mask) == 0)
        c = KEY_COLOR_16BIT;
    }
  }
  else if (COLOR_FORMAT_TYPE_IS_OVERLAY (input)) {
    if (COLOR_FORMAT_TYPE_IS_ALPHA (output)) {
      /* Swap the topbit. */
      c = c ^ o_mask;
    }
    else if (!COLOR_FORMAT_TYPE_IS_ALPHA (output)
             && !COLOR_FORMAT_TYPE_IS_OVERLAY (output)) {
      /* Set to key color when overlay is high.  */
      if (key && ((c & o_mask) == o_mask))
        c = KEY_COLOR_16BIT;
    }
  }
  else if (!COLOR_FORMAT_TYPE_IS_ALPHA (input)
           && !COLOR_FORMAT_TYPE_IS_OVERLAY (input)) {
    if (COLOR_FORMAT_TYPE_IS_ALPHA (output)) {
      if (key && c == KEY_COLOR_16BIT)
        c = argb_transparent;
      else
        c = (c | a_mask) & argb_mask;
    }
    else if (COLOR_FORMAT_TYPE_IS_OVERLAY (output)) {
      if (key && c == KEY_COLOR_16BIT)
        c = orgb_transparent;
      else
        c = (c & ~o_mask) & orgb_mask;
    }
  }

  if (COLOR_FORMAT_TYPE_IS_RGB (input) != COLOR_FORMAT_TYPE_IS_RGB (output)) {
    /* swap r and b */
    uint32_t c2 = ((c & 0b1000001111100000) | (c & 0b0111110000000000) >> 10
                   | (c & 0b0000000000011111) << 10);
    c = c2;
  }
  return c;
}

static uint32_t
upconvert_16_32 (uint32_t c, color_format_t input, color_format_t output)
{
  bool overlay = false;
  uint32_t r, g, b, c2 = 0;

  if (COLOR_FORMAT_TYPE_IS_OVERLAY (input) && (c & 0b1000000000000000))
    overlay = true;
  if (COLOR_FORMAT_TYPE_IS_RGB (input)) {
    r = (c & 0b0111110000000000) >> 7;
    g = (c & 0b0000001111100000) >> 2;
    b = (c & 0b0000000000011111) << 3;
  }
  if (COLOR_FORMAT_TYPE_IS_BGR (input)) {
    r = (c & 0b0000000000011111) << 3;
    g = (c & 0b0000001111100000) >> 2;
    b = (c & 0b0111110000000000) >> 7;
  }
  if (COLOR_FORMAT_TYPE_IS_RGB (output))
    c2 = r << 16 | g << 8 | b;
  if (COLOR_FORMAT_TYPE_IS_BGR (output))
    c2 = r | g << 8 | b << 16;
  if (COLOR_FORMAT_TYPE_IS_ALPHA (output)) {
    if (!overlay)
      c2 = c2 | 0xff000000;
  }
  return c2;
}

static uint32_t
downconvert_32_16 (uint32_t c, color_format_t input, color_format_t output)
{
  uint32_t c2 = 0;
  uint32_t r, g, b;
  if (COLOR_FORMAT_TYPE_IS_ALPHA (input)
      && COLOR_FORMAT_TYPE_IS_OVERLAY (output)) {
    /* We call 50% alpha or less as 'overlay' */
    if (!(c & 0x80000000))
      c2 = 0b1000000000000000;
  }
  else if (COLOR_FORMAT_TYPE_IS_ALPHA (input)
           && COLOR_FORMAT_TYPE_IS_ALPHA (output)) {
    /* We call 50% alpha or more as alpha */
    if (c & 0x80000000)
      c2 = 0b1000000000000000;
  }
  if (COLOR_FORMAT_TYPE_IS_RGB (input)) {
    r = (c & 0x00f80000) >> 19;
    g = (c & 0x0000f800) >> 11;
    b = (c & 0x000000f8) >> 3;
  }
  if (COLOR_FORMAT_TYPE_IS_BGR (input)) {
    r = (c & 0x000000f8) >> 3;
    g = (c & 0x0000f800) >> 11;
    b = (c & 0x00f80000) >> 19;
  }
  if (COLOR_FORMAT_TYPE_IS_RGB (output))
    c2 |= r << 10 | g << 5 | b;
  if (COLOR_FORMAT_TYPE_IS_BGR (output))
    c2 |= r | g << 5 | b << 10;
  return c2;
}

void
colorval_to_rgba (uint8_t *c, color_format_t cf,
                  unsigned int *r, unsigned int *g, unsigned int *b,
                  unsigned int *a)
{
  unsigned int bpp = COLOR_FORMAT_BPP (cf);
  switch (bpp) {
  case 16:
    if (COLOR_FORMAT_TYPE_IS_RGB (cf))
      PTR_UNPACK16 (c, *a, *r, *g, *b);
    if (COLOR_FORMAT_TYPE_IS_BGR (cf))
      PTR_UNPACK16 (c, *a, *b, *g, *r);
    if (!COLOR_FORMAT_TYPE_IS_OVERLAY (cf))
      *a = 0;
    break;
  case 24:
    if (COLOR_FORMAT_TYPE_IS_RGB (cf))
      PTR_UNPACK24 (c, *r, *g, *b);
    if (COLOR_FORMAT_TYPE_IS_BGR (cf))
      PTR_UNPACK24 (c, *b, *g, *r);
    *a = 0;
    break;
  case 32:
    if (COLOR_FORMAT_TYPE_IS_RGB (cf))
      PTR_UNPACK32 (c, *a, *r, *g, *b);
    if (COLOR_FORMAT_TYPE_IS_BGR (cf))
      PTR_UNPACK32 (c, *a, *b, *g, *r);
    if (!COLOR_FORMAT_TYPE_IS_ALPHA (cf))
      *a = 0;
    break;
  }
}

void
rgba_to_colorval (uint8_t * c, color_format_t cf,
                  unsigned int r, unsigned int g, unsigned int b,
                  unsigned int a)
{
  unsigned int bpp = COLOR_FORMAT_BPP (cf);

  if (bpp == 16) {
    uint16_t c2;
    if (COLOR_FORMAT_TYPE_IS_RGB (cf))
      c2 = r << 10 | g << 5 | b;
    if (COLOR_FORMAT_TYPE_IS_BGR (cf))
      c2 = r | g << 5 | b << 10;
    c2 |= a << 15;
    c[0] = c2 & 0xff;
    c[1] = c2 / 256;
  }
  if (bpp == 24 || bpp == 32) {
    uint32_t c2;
    if (COLOR_FORMAT_TYPE_IS_RGB (cf))
      c2 = r << 16 | g << 8 | b;
    if (COLOR_FORMAT_TYPE_IS_BGR (cf))
      c2 = r | g << 8 | b << 16;
    if (COLOR_FORMAT_TYPE_IS_ALPHA (cf))
      c2 |= 0xff000000;
    c[0] = c2 & 0xff;
    c[1] = (c2 & 0xff00) >> 8;
    c[2] = (c2 & 0xff0000) >> 16;
    if (bpp == 32)
      c[3] = (c2 & 0xff000000) >> 24;
  }
}
