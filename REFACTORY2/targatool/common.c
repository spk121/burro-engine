#include <glib.h>
#include "common.h"

static guint32 convert32 (guint32 c, color_format_t input, color_format_t output);
static guint32 convert16 (guint32 c, color_format_t input, color_format_t output, gboolean key);
static guint32 upconvert_16_32 (guint32 c, color_format_t input, color_format_t output);
static guint32 downconvert_32_16 (guint32 c, color_format_t input, color_format_t output);

guint32
convert_color (guint32 c, color_format_t input, color_format_t output)
{
  guint bpp_in = COLOR_FORMAT_BPP (input);
  guint bpp_out = COLOR_FORMAT_BPP (output);

  if (input == output)
    return c;
  if (bpp_in >= 24 && bpp_out >= 24)
    return convert32 (c, input, output);
  if (bpp_in == 16 && bpp_out == 16)
    return convert16 (c, input, output, FALSE);
  else if (bpp_in == 16 && bpp_out >= 24)
    return upconvert_16_32 (c, input, output);
  else if (bpp_in >= 24 && bpp_out == 16)
    return downconvert_32_16 (c, input, output);

  g_return_val_if_reached (0);
}

void
convert_color_array (guint8 * out_arr, guint8 * in_arr, guint32 count, color_format_t in_cf, color_format_t out_cf)
{
  guint i;
  int in_bpp, out_bpp;
  guint32 in_c, out_c;

  in_bpp = COLOR_FORMAT_BPP (in_cf);
  out_bpp = COLOR_FORMAT_BPP (out_cf);

  for (i = 0; i < count; i++)
    {
      switch (BIT_TO_BYTE (in_bpp))
	{
	case 2:
	  in_c = (in_arr[i * 2] + (in_arr[i * 2 + 1] << 8));
	  break;
	case 3:
	  in_c = (in_arr[i * 3] + (in_arr[i * 3 + 1] << 8) + (in_arr[i * 3 + 2] << 16));
	  break;
	case 4:
	  in_c = (in_arr[i * 4] + (in_arr[i * 4 + 1] << 8) + (in_arr[i * 4 + 2] << 16) + (in_arr[i * 4 + 3] << 24));
	  break;
	}
      out_c = convert_color (in_c, in_cf, out_cf);
      switch (BIT_TO_BYTE (out_bpp))
	{
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

static guint32
convert32 (guint32 c, color_format_t input, color_format_t output)
{
  if (!COLOR_FORMAT_TYPE_IS_ALPHA (input) && COLOR_FORMAT_TYPE_IS_ALPHA (output))
    {
      /* Add 0xFF for fully opaque alpha.  */
      c = c | 0xff000000;
    }
  if (COLOR_FORMAT_TYPE_IS_ALPHA (input) && !COLOR_FORMAT_TYPE_IS_ALPHA (output))
    {
      /* Strip alpha.  */
      c = c & 0x00ffffff;
      /* FIXME: turn fully transparent into a key color? */
    }
  if (COLOR_FORMAT_TYPE_IS_RGB (input) != COLOR_FORMAT_TYPE_IS_RGB (output))
    {
      /* Swap r and b */
      guint32 c2 = (c & 0xff00ff00) | (c & 0x00ff0000) >> 16 | (c & 0x000000ff) << 16;
      c = c2;
    }
  return c;
}

static guint32
convert16 (guint32 c, color_format_t input, color_format_t output, gboolean key)
{
  const guint32 rgb_mask = 0b0111111111111111;
  const guint32 o_mask = 0b1000000000000000;
  const guint32 a_mask = 0b1000000000000000;
  const guint32 orgb_mask = 0b1111111111111111;
  const guint32 argb_mask = 0b1111111111111111;
  const guint32 argb_transparent = 0b0000000000000000;
  const guint32 orgb_transparent = 0b1000000000000000;
  /*
     OUTPUT
     | OUTPUT ARGB    | OUTPUT ORGB       | OUTPUT xRGB
     ----------------------------------- ----------------------------
     INPUT ARGB    | keep alpha     | swap alpha        | low alpha -> key
     INPUT ORGB    | swap overlay   | keep overlay      | high overlay -> key
     INPUT xRGB    | key->low alpha | key->high overlay | keep

   */
  if (COLOR_FORMAT_TYPE_IS_ALPHA (input))
    {
      if (COLOR_FORMAT_TYPE_IS_OVERLAY (output))
	{
	  /* Swap the topbit. */
	  c = c ^ a_mask;
	}
      else if (!COLOR_FORMAT_TYPE_IS_ALPHA (output) && !COLOR_FORMAT_TYPE_IS_OVERLAY (output))
	{
	  /* Set to key color when alpha is low.  */
	  if (key && (c & a_mask) == 0)
	    c = KEY_COLOR_16BIT;
	}
    }
  else if (COLOR_FORMAT_TYPE_IS_OVERLAY (input))
    {
      if (COLOR_FORMAT_TYPE_IS_ALPHA (output))
	{
	  /* Swap the topbit. */
	  c = c ^ o_mask;
	}
      else if (!COLOR_FORMAT_TYPE_IS_ALPHA (output) && !COLOR_FORMAT_TYPE_IS_OVERLAY (output))
	{
	  /* Set to key color when overlay is high.  */
	  if (key && ((c & o_mask) == o_mask))
	    c = KEY_COLOR_16BIT;
	}
    }
  else if (!COLOR_FORMAT_TYPE_IS_ALPHA (input) && !COLOR_FORMAT_TYPE_IS_OVERLAY (input))
    {
      if (COLOR_FORMAT_TYPE_IS_ALPHA (output))
	{
	  if (key && c == KEY_COLOR_16BIT)
	    c = argb_transparent;
	  else
	    c = (c | a_mask) & argb_mask;
	}
      else if (COLOR_FORMAT_TYPE_IS_OVERLAY (output))
	{
	  if (key && c == KEY_COLOR_16BIT)
	    c = orgb_transparent;
	  else
	    c = (c & ~o_mask) & orgb_mask;
	}
    }

  if (COLOR_FORMAT_TYPE_IS_RGB (input) != COLOR_FORMAT_TYPE_IS_RGB (output))
    {
      /* swap r and b */
      guint32 c2 = (c & 0b1000001111100000) | (c & 0b0111110000000000) >> 10 | (c & 0b0000000000011111) << 10;
      c = c2;
    }
  return c;
}

static guint32
upconvert_16_32 (guint32 c, color_format_t input, color_format_t output)
{
  gboolean overlay = FALSE;
  guint32 r, g, b, c2 = 0;

  if (COLOR_FORMAT_TYPE_IS_OVERLAY (input) && (c & 0b1000000000000000))
    overlay = TRUE;
  if (COLOR_FORMAT_TYPE_IS_RGB (input))
    {
      r = (c & 0b0111110000000000) >> 7;
      g = (c & 0b0000001111100000) >> 2;
      b = (c & 0b0000000000011111) << 3;
    }
  if (COLOR_FORMAT_TYPE_IS_BGR (input))
    {
      r = (c & 0b0000000000011111) << 3;
      g = (c & 0b0000001111100000) >> 2;
      b = (c & 0b0111110000000000) >> 7;
    }
  if (COLOR_FORMAT_TYPE_IS_RGB (output))
    c2 = r << 16 | g << 8 | b;
  if (COLOR_FORMAT_TYPE_IS_BGR (output))
    c2 = r | g << 8 | b << 16;
  if (COLOR_FORMAT_TYPE_IS_ALPHA (output))
    if (!overlay)
      c2 = c2 | 0xff000000;
  return c2;
}

static guint32
downconvert_32_16 (guint32 c, color_format_t input, color_format_t output)
{
  guint32 c2 = 0;
  guint32 r, g, b;
  if (COLOR_FORMAT_TYPE_IS_ALPHA (input) && COLOR_FORMAT_TYPE_IS_OVERLAY (output))
    {
      /* We call 50% alpha or less as 'overlay' */
      if (!(c & 0x80000000))
	c2 = 0b1000000000000000;
    }
  else if (COLOR_FORMAT_TYPE_IS_ALPHA (input) && COLOR_FORMAT_TYPE_IS_ALPHA (output))
    {
      /* We call 50% alpha or more as alpha */
      if (c & 0x80000000)
	c2 = 0b1000000000000000;
    }
  if (COLOR_FORMAT_TYPE_IS_RGB (input))
    {
      r = (c & 0x00f80000) >> 19;
      g = (c & 0x0000f800) >> 11;
      b = (c & 0x000000f8) >> 3;
    }
  if (COLOR_FORMAT_TYPE_IS_BGR (input))
    {
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
colorval_to_rgba (guint8 * c, color_format_t cf, guint * r, guint * g, guint * b, guint * a)
{
  guint bpp = COLOR_FORMAT_BPP (cf);
  switch (bpp)
    {
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
rgba_to_colorval (guint8 * c, color_format_t cf, guint r, guint g, guint b, guint a)
{
  guint bpp = COLOR_FORMAT_BPP (cf);

  if (bpp == 16)
    {
      guint16 c2;
      if (COLOR_FORMAT_TYPE_IS_RGB (cf))
	c2 = r << 10 | g << 5 | b;
      if (COLOR_FORMAT_TYPE_IS_BGR (cf))
	c2 = r | g << 5 | b << 10;
      c2 |= a << 15;
      c[0] = c2 & 0xff;
      c[1] = c2 / 256;
    }
  if (bpp == 24 || bpp == 32)
    {
      guint32 c2;
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
