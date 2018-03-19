#include <glib.h>
#include <stdio.h>
#include "targa.h"

/*  First, we take anything with alpha < 50% and make it clear.
 *
 * In the palette, we reserve slot 255 for fully transparent.
 *
 * Then, imagine R,G,B as being a 3-space, with each axis ranging from
 * 0 to 32 (5 bits).  We initially divide that 3-space into 8
 * parallelograms, The first one could contain the low R, low G, and
 * low B values.  The second could contain the high R, low G and low B
 * values, etc.
 * 
 * We compute the population of each bin.  The most populus bin is then
 * itself divided into 8 bins.  So we've gone from 8 bins to 15.
 *
 * The process repeats itself: 8, 15, 22, 29 ... 253. After the 
 * initial division, there are 35 iterations.
 *
 * We go back then and map each transparent pixel to the transparent
 * color index and each color pixels to its closest match. 
 *
 * For each color bit, the final palette color is the average of the colors
 * of the pixels in that bin. */

#define MAX_ITERATIONS (35)
#define MAX_BINS (256);

typedef struct pal_box_tag
{
  int r_lo, r_hi, g_lo, g_hi, b_lo, b_hi;
  int count;
  int r_sum, g_sum, b_sum;
} pal_box_t;

gint box_compare_count (gconstpointer a, gconstpointer b);
void update_box_populations (GPtrArray *boxes,
			     guint8 *argb,
			     int n,
			     color_format_t cf);
void subdivide_last_box (GPtrArray *boxes);
void find_box_index (GPtrArray *boxes,
		     guint8 *argb,
		     int n,
		     color_format_t cf,
		     guint8 *index);
void get_palette (GPtrArray *boxes, guint8 *p, color_format_t cf);
void colorval_to_rgba (guint8 *c, color_format_t cf, guint *r, guint *g, guint *b, guint *a);
void rgba_to_colorval (guint8 *c, color_format_t cf, guint r, guint g, guint b, guint a);
static void palettize (guint8 *argb, gint n, color_format_t cf, guint8 *index, guint8 *palette);

targa_error_t targa_convert_to_nonindexed (targa_image_t *t)
{
  color_format_t color;
  guint bits_per_pixel;
  guint bytes_per_pixel;
  guint width, height;
  guint8 index, offset;
  guint8 *img;
  guint i,b;
	
  // FIXME: should also convert grayscale images here

  if (targa_has_palette (t))
    {
      color = targa_get_palette_color_format (t);
      bits_per_pixel = COLOR_FORMAT_BPP(color);
      bytes_per_pixel = BIT_TO_BYTE(bits_per_pixel);

      targa_get_image_dimensions (t, &width, &height);
      img = g_new (guint8, width * height * bytes_per_pixel);
      offset = targa_get_color_map_first_index (t);
      for (i = 0; i < width * height; i ++)
	{
	  index = t->data.image_data[i + offset];
	  for (b = 0; b < bytes_per_pixel; b++)
	    img[i * bytes_per_pixel + b] = t->data.color_map_data[index * bytes_per_pixel + b];
	}
		
      t->header.image_type = TARGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR;
      t->header.pixel_depth = bits_per_pixel;

      t->header.color_map_type = 0;
      t->header.color_map_first_entry_index = 0;
      t->header.color_map_entry_size = 0;
      t->header.color_map_length = 0;

      g_free (t->data.color_map_data);
      g_free (t->data.image_data);

      t->data.color_map_data = NULL;
      t->data.image_data = img;
    }
  return TARGA_OK;
}

targa_error_t targa_convert_to_indexed (targa_image_t *t)
{
  color_format_t color;
  guint bits_per_pixel;
  guint bytes_per_pixel;
  guint width, height;
  guint8 *palette, *indices;

  // FIXME: should also convert grayscale images here
  if (!targa_has_palette (t))
    {
      color = targa_get_image_color_format (t);
      bits_per_pixel = COLOR_FORMAT_BPP (color);
      bytes_per_pixel = BIT_TO_BYTE(bits_per_pixel);
      targa_get_image_dimensions (t, &width, &height);

      palette = g_new (guint8, 256 * bytes_per_pixel);
      indices = g_new (guint8, width * height);
      palettize (t->data.image_data, width*height, color, indices, palette);

      t->header.image_type = TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED;
      t->header.color_map_type = 1;
      t->header.color_map_first_entry_index = 0;
      t->header.color_map_length = 256;
      t->header.color_map_entry_size = bits_per_pixel;
      t->header.pixel_depth = 8;

      g_free (t->data.color_map_data);
      g_free (t->data.image_data);
      t->data.image_data = indices;
      t->data.color_map_data = palette;
    }
  return TARGA_OK;
}

targa_error_t targa_convert_to_color (targa_image_t *t, color_format_t output_cf)
{
  color_format_t input_cf;
  guint width, height;
  int output_bits_per_entry, output_bytes_per_entry;
  guint8 *palette_out, *img_out;

  if (targa_has_palette (t))
    {
      input_cf = targa_get_palette_color_format (t);
      output_bits_per_entry = COLOR_FORMAT_BPP (output_cf);
      output_bytes_per_entry = BIT_TO_BYTE (output_bits_per_entry);
      palette_out = g_new (guint8,
			   t->header.color_map_length * output_bytes_per_entry);
      convert_color_array (palette_out,
			   t->data.color_map_data,
			   t->header.color_map_length,
			   input_cf,
			   output_cf);

      targa_set_palette_color_format (t, output_cf);
      g_free (t->data.color_map_data);
      t->data.color_map_data = palette_out;
    }
  else
    {
      input_cf = targa_get_image_color_format (t);
      output_bits_per_entry = COLOR_FORMAT_BPP (output_cf);
      output_bytes_per_entry = BIT_TO_BYTE (output_bits_per_entry);
      targa_get_image_dimensions (t, &width, &height);
      img_out = g_new (guint8, width * height * output_bytes_per_entry);
      convert_color_array (img_out,
			   t->data.image_data,
			   width * height,
			   input_cf,
			   output_cf);
      targa_set_image_color_format (t, output_cf);
      g_free (t->data.image_data);
      t->data.image_data = img_out;
    }
  return TARGA_OK;
}

static void palettize (guint8 *argb, gint n, color_format_t cf, guint8 *index, guint8 *palette)
{
  GPtrArray *boxes;
  pal_box_t *box;
  int r,g,b;
  int range;
  int iter;
  int bits_per_color;

  bits_per_color = COLOR_FORMAT_RGB(cf);
  if (bits_per_color > 8)
    bits_per_color = 8;

  range = 1 << bits_per_color;
  boxes = g_ptr_array_sized_new (255);
  for (b = 0; b < range; b += range / 2)
    {
      for (g = 0; g < range; g += range / 2)
	{
	  for (r = 0; r < range; r += range / 2)
	    {
	      box = g_new0 (pal_box_t, 1);
	      box->r_lo = r;
	      box->r_hi = r + (range / 2) - 1;
	      box->g_lo = g;
	      box->g_hi = g + (range / 2) - 1;
	      box->b_lo = b;
	      box->b_hi = b + (range / 2) - 1;
	      box->count = 0;
	      g_ptr_array_add (boxes, box);
	    }
	}
    }


  for (iter = 0; iter < MAX_ITERATIONS; iter ++)
    {
      update_box_populations (boxes, argb, n, cf);
      g_ptr_array_sort (boxes, box_compare_count);
      subdivide_last_box (boxes);

#if 0
      g_ptr_array_sort (boxes, box_compare_count);
      {
	int i;


	update_box_populations (boxes, argb, n, bits_per_pixel);
	for (i = 0; i < boxes->len; i ++)
	  {
	    pal_box_t *in = g_ptr_array_index (boxes, i);
	    printf ("%d %d count %d red %d %d green %d %d blue %d %d\n",
		    iter, i, in->count, in->r_lo, in->r_hi, in->g_lo,
		    in->g_hi, in->b_lo, in->g_hi);
	  }
      }
#endif
    }
  find_box_index (boxes, argb, n, cf, index);
  get_palette (boxes, palette, cf);
}

void get_palette (GPtrArray *boxes, guint8 *p, color_format_t cf)
{
  guint i;
  guint r, b, g;
  guint bytes_per_pixel = BIT_TO_BYTE(COLOR_FORMAT_BPP(cf));

  /* Always put black and white at the beginning */
  if (bytes_per_pixel == 2)
    {
      PTR_PACK16(p,0,0,0,0);
      PTR_PACK16(p+bytes_per_pixel,0,31,31,31);
    }
  else if (bytes_per_pixel == 3)
    {
      PTR_PACK24(p,0,0,0);
      PTR_PACK24(p+bytes_per_pixel,255,255,255);
    }
  else
    {
      if (COLOR_FORMAT_TYPE_IS_ALPHA(cf))
	{
	  PTR_PACK32(p,255,0,0,0);
	  PTR_PACK32(p+bytes_per_pixel,255,255,255,255);
	}
      else
	{
	  PTR_PACK32(p,0,0,0,0);
	  PTR_PACK32(p+bytes_per_pixel,0,255,255,255);
	}
    }

  /* Then up to 253 more colors */
  for (i = 0; i < boxes->len; i ++)
    {
      pal_box_t *box = g_ptr_array_index (boxes, i);
      if (box->count == 0)
	{
	  r = (box->r_lo + box->r_hi) / 2;
	  g = (box->g_lo + box->g_hi) / 2;
	  b = (box->b_lo + box->b_hi) / 2;
	}
      else
	{
	  r = box->r_sum / box->count;
	  g = box->g_sum / box->count;
	  b = box->b_sum / box->count;
	}
      if (bytes_per_pixel == 2)
	PTR_PACK16(p + (i+2) * bytes_per_pixel,0,b,g,r);
      else if (bytes_per_pixel == 3)
	PTR_PACK24(p + (i+2) * bytes_per_pixel,b,g,r);
      else
	{
	  if (COLOR_FORMAT_TYPE_IS_ALPHA(cf))
	    PTR_PACK32(p + (i+2) * bytes_per_pixel,255,b,g,r);
	  else
	    PTR_PACK32(p + (i+2) * bytes_per_pixel,0,b,g,r);
	}
    }

  /* Set the last color in the palette to be magenta! */
  if (bytes_per_pixel == 2)
    PTR_PACK16(p + 255 * bytes_per_pixel,0,31,0,31);
  else if (bytes_per_pixel == 3)
    PTR_PACK24(p + 255 * bytes_per_pixel,255,0,255);
  else
    PTR_PACK32(p + 255 * bytes_per_pixel,0,255,0,255);
}

gint box_compare_count (gconstpointer a, gconstpointer b)
{
  pal_box_t * const *box_a = a;
  pal_box_t * const *box_b = b;
  return ((*box_a)->count - (*box_b)->count);
}

void update_box_populations (GPtrArray *boxes, guint8 *argb, int n, color_format_t cf)
{
  gint i;
  gint r, g, b, a, o;
  int bytes_per_pixel = BIT_TO_BYTE(COLOR_FORMAT_BPP(cf));
  int bits_per_color = COLOR_FORMAT_RGB(cf);

  /* Reset the historgram data */
  for (guint j = 0; j < boxes->len; j ++)
    {
      pal_box_t *box = g_ptr_array_index (boxes, j);
      box->count = 0;
      box->r_sum = 0;
      box->g_sum = 0;
      box->b_sum = 0;
    }

  /* Fill the histogram data */
  for (gint j = 0; j < n; j ++)
    {
      if (bytes_per_pixel == 2)
	PTR_UNPACK16(&argb[j*bytes_per_pixel],o,b,g,r);
      else if (bytes_per_pixel == 3)
	PTR_UNPACK24(&argb[j*bytes_per_pixel],b,g,r);
      else
	PTR_UNPACK32(&argb[j*bytes_per_pixel],a,b,g,r);
      i = boxes->len - 1;
      while (1)
	{
	  pal_box_t *box = g_ptr_array_index (boxes, i);
	  if (r >= box->r_lo && r <= box->r_hi
	      && b >= box->b_lo && b <= box->b_hi
	      && g >= box->g_lo && g <= box->g_hi)
	    {
	      box->count ++;
	      box->r_sum += r;
	      box->g_sum += g;
	      box->b_sum += b;
	      break;
	    }
	  else
	    {
	      if (i == 0)
		break;
	      i --;
	    }
	}
    }
}

void
find_box_index (GPtrArray *boxes, guint8 *argb, int n, color_format_t cf, guint8 *index)
{
  int i;
  gint r, g, b, a, o;
  int bytes_per_pixel = BIT_TO_BYTE(COLOR_FORMAT_BPP(cf));
  int bits_per_color = COLOR_FORMAT_RGB(cf);
  int max;
  max = (1 << bits_per_color) - 1;

  for (int j = 0; j < n; j ++)
    {
      o = 0;
      a = 0;
      if (bytes_per_pixel == 2)
	PTR_UNPACK16(&argb[j*bytes_per_pixel],o,b,g,r);
      else if (bytes_per_pixel == 3)
	PTR_UNPACK24(&argb[j*bytes_per_pixel],b,g,r);
      else
	PTR_UNPACK32(&argb[j*bytes_per_pixel],a,b,g,r);

      /* We're using index 255 to hold complete transparency. */
      if ((COLOR_FORMAT_TYPE_IS_OVERLAY(cf) && o == 1) 
	  || (COLOR_FORMAT_TYPE_IS_ALPHA(cf) && (a < 127)))
	{
	  index[j] = 255;
	  continue;
	}

      /* index zero holds black and index 1 holds white */
      if (r == 0 && g == 0 && b == 0)
	{
	  index[j] = 0;
	  continue;
	}
      if (r == max && g == max && b == max)
	{
	  index[j] = 1;
	  continue;
	}
		
      i = 0;
      while (1)
	{
	  pal_box_t *box = g_ptr_array_index (boxes, i);
	  if (r >= box->r_lo && r <= box->r_hi
	      && b >= box->b_lo && b <= box->b_hi
	      && g >= box->g_lo && g <= box->g_hi)
	    {
	      index[j] = i + 2;
	      break;
	    }
	  else
	    {
	      i ++;
	      if (i >= boxes->len)
		{
		  g_warning ("colorval somehow not in colorspace");
		  index[j] = 255;
		  break;
		}
	    }
	}
    }
}
		
void subdivide_last_box (GPtrArray *boxes)
{
  pal_box_t *in;
  pal_box_t *box;
  int r, g, b;
  int dr, dg, db;
  int i;

  i = boxes->len - 1;
  while (i >= 0)
    {
      in = g_ptr_array_index (boxes, i);
      if (in->b_hi == in->b_lo
	  && in->g_hi == in->g_lo
	  && in->r_hi == in->r_lo)
	{
	  i --;
	}
      else
	break;
    }

  db = (in->b_hi - in->b_lo + 1) / 2;
  dg = (in->g_hi - in->g_lo + 1) / 2;
  dr = (in->r_hi - in->r_lo + 1) / 2;
  if (db < 1) db = 1;
  if (dg < 1) dg = 1;
  if (dr < 1) dr = 1;
	
  g_ptr_array_remove_index (boxes, i);
  for (b = in->b_lo; b <= in->b_hi; b += db)
    for (g = in->g_lo; g <= in->g_hi; g += dg)
      for (r = in->r_lo; r <= in->r_hi; r += dr)
	{
	  box = g_new0 (pal_box_t, 1);
	  box->r_lo = r;
	  box->r_hi = r + dr - 1;
	  box->g_lo = g;
	  box->g_hi = g + dg - 1;
	  box->b_lo = b;
	  box->b_hi = b + db - 1;
	  box->count = 0;
	  g_ptr_array_add (boxes, box);
	}
}

