#include <memory.h>
#include <stdint.h>
#include <stdbool.h>
#include <SDL.h>
#include <cairo.h>
#include "console.h"
#include "fixed8x12.h"

#define COMPOSE(render,codepoint) (((uint32_t)(render) << 16)|(codepoint))
#define RENDERING(x) ((uint16_t)(((x) & 0xFFFF0000) >> 16))
#define CODEPOINT(x) ((uint16_t)((x) & 0x0000FFFF))

#define CONSOLE_NUM_COLORS 10
#define CONSOLE_NUM_INTENSITIES 3

static const uint32_t fg_palette[CONSOLE_NUM_COLORS *
				 CONSOLE_NUM_INTENSITIES] = {
/*                   normal     faint        bold        */
/* default       */ 0xffeeee88, 0xff888888, 0xffffff88,
  /* black       */ 0xff000000, 0xff000000, 0xff000000,
  /* red         */ 0xffee0000, 0xff880000, 0xffff0000,
  /* green       */ 0xff00ee00, 0xff008800, 0xff00ff00,
  /* yellow      */ 0xffeeee00, 0xff888800, 0xffffff00,
  /* blue        */ 0xff0000ee, 0xff000088, 0xff0000ff,
  /* magenta     */ 0xffee00ee, 0xff880088, 0xffff00ff,
  /* cyan        */ 0xff00eeee, 0xff008888, 0xff00ffff,
  /* white       */ 0xffeeeeee, 0xff888888, 0xffffffff,
  /* transparent */ 0x00000000, 0x00000000, 0x00000000,
};

static const uint32_t bg_palette[CONSOLE_NUM_COLORS] = {
/*                   background        */
/* default       */ 0x00000000,
  /* black       */ 0xff000000,
  /* red         */ 0xffee0000,
  /* green       */ 0xff00ee00,
  /* yellow      */ 0xffeeee00,
  /* blue        */ 0xff0000ee,
  /* magenta     */ 0xffee00ee,
  /* cyan        */ 0xff00eeee,
  /* white       */ 0xffeeeeee,
  /* transparent */ 0x00000000,
};

static int row, col;
static uint16_t rendition;
static uint32_t cells[CONSOLE_ROWS * CONSOLE_COLS];
static bool cursor_visible = TRUE;

void
console_reset (void)
{
  int dummy;
  row = 0;
  col = 0;
  rendition = 0;
  cursor_visible = TRUE;
  memset (cells, 0, sizeof (cells));
}

static void
set_rendition (uint16_t val, uint16_t mask)
{
  rendition &= ~mask;
  rendition |= (val & mask);
}

void
console_set_bgcolor (uint32_t c)
{
  set_rendition (c, COLOR_BG_MASK);
}

void
console_set_blink (uint32_t c)
{
  set_rendition (c, BLINK_MASK);
}

void
console_set_intensity (uint32_t c)
{
  set_rendition (c, INTENSITY_MASK);
}

void
console_set_fgcolor (uint32_t c)
{
  set_rendition (c, COLOR_FG_MASK);
}

void
console_set_polarity (uint32_t c)
{
  set_rendition (c, POLARITY_MASK);
}

void
console_set_underline (uint32_t c)
{
  set_rendition (c, UNDERLINE_MASK);
}

void
console_set_default (uint32_t c)
{
  rendition = 0;
}

void
console_set_cursor_visiblity (bool flag)
{
  cursor_visible = flag;
}

// Deleting removes characters left of the cursor and then pulls the
// remaining characters in to fill the gap.
void
console_delete_left (int n)
{
  SDL_assert (n > 0);

  // Note that 'start' is inclusive, but 'end'
  // is exclusive, e.g., one past the end of the region.

  int delete_region_start = row * CONSOLE_COLS + col - (n - 1);
  int delete_region_end = row * CONSOLE_COLS + col + 1;
  int source_region_start = row * CONSOLE_COLS;
  int source_region_end = delete_region_start;
  int destination_region_start = row * CONSOLE_COLS + n;
  int fill_region_start = source_region_start;
  int fill_region_end = destination_region_start;

  memmove (cells + destination_region_start,
	   cells + source_region_start,
	   sizeof (uint32_t) * (source_region_end - source_region_start));
  memset (cells + fill_region_start, 0,
	  sizeof (uint32_t) * (fill_region_end - fill_region_start));
}

// Deleting removes characters right of the cursor and then pulls the
// remaining characters in to fill the gap.
void
console_delete_right (int n)
{
  SDL_assert (n > 0);

  // Note that 'start' is inclusive, but 'end'
  // is exclusive, e.g., one past the end of the region.

  int delete_region_start = row * CONSOLE_COLS + col;
  int delete_region_end = row * CONSOLE_COLS + col + n;
  int source_region_start = delete_region_end;
  int source_region_end = (row + 1) * CONSOLE_COLS;
  int destination_region_start = delete_region_start;
  int fill_region_start = source_region_end - n;
  int fill_region_end = source_region_end;

  memmove (cells + destination_region_start,
	   cells + source_region_start,
	   source_region_end - source_region_start);
  memset (cells + fill_region_start, 0, fill_region_end - fill_region_start);
}

// Deletes all off current line plus n-1 previous lines
void
console_delete_line_up (int n)
{
  SDL_assert (n > 0);

  // Note that 'start' is inclusive, but 'end'
  // is exclusive, e.g., one past the end of the region.

  int delete_region_start = (row - (n - 1)) * CONSOLE_COLS;
  int delete_region_end = (row + 1) * CONSOLE_COLS;
  int source_region_start = 0;
  int source_region_end = delete_region_start;
  int destination_region_start = n * CONSOLE_COLS;
  int fill_region_start = source_region_start;
  int fill_region_end = destination_region_start;

  memmove (cells + destination_region_start,
	   cells + source_region_start,
	   source_region_end - source_region_start);
  memset (cells + fill_region_start, 0, fill_region_end - fill_region_start);
}


void
console_delete_line_down (int n)
{
  SDL_assert (n > 0);

  // Note that 'start' is inclusive, but 'end'
  // is exclusive, e.g., one past the end of the region.

  int delete_region_start = row * CONSOLE_COLS;
  int delete_region_end = (row + n) * CONSOLE_COLS;
  int source_region_start = delete_region_end;
  int source_region_end = CONSOLE_ROWS * CONSOLE_COLS;
  int destination_region_start = delete_region_start;
  int fill_region_start = (CONSOLE_ROWS - n) * CONSOLE_COLS;
  int fill_region_end = CONSOLE_ROWS * CONSOLE_COLS;

  // Delete the current line and the n-1 following lines.
  // by moving the rest of the buffer up to fill the gap.
  memmove (cells + destination_region_start,
	   cells + source_region_start,
	   source_region_end - source_region_start);

  // Set everything beyond the copied region to the erased state.
  memset (cells + fill_region_start, 0, fill_region_end - fill_region_start);
}

void
console_erase_left (int n)
{
  SDL_assert (n > 0);

  // Note that 'start' is inclusive, but 'end'
  // is exclusive, e.g., one past the end of the region.

  int erase_region_start = row * CONSOLE_COLS + col - (n - 1);
  int erase_region_end = row * CONSOLE_COLS + col + 1;

  memset (cells + erase_region_start, 0,
	  erase_region_end - erase_region_start);
}

void
console_erase_right (int n)
{
  SDL_assert (n > 0);

  if (col + n > CONSOLE_COLS)
    n = CONSOLE_COLS - col;
  // Note that 'start' is inclusive, but 'end'
  // is exclusive, e.g., one past the end of the region.

  int erase_region_start = row * CONSOLE_COLS + col;
  int erase_region_end = row * CONSOLE_COLS + col + n;

  memset (cells + erase_region_start, 0,
	  erase_region_end - erase_region_start);
}


// Write the codepoint at the current location.  If IRM is 1 aka
// Insert Mode, this codepoint is being inserted in the row, and isn't
// overwriting a character.  If HEM is zero, character motion is to
// the right.  If HEM is 1, push them left.
// HOME and LIMIT bracket the valid character area.
void
console_write_char (uint16_t codepoint, int irm, int hem, int home, int limit)
{
  if (col < home && col > limit)
    return;

  if (irm == 1 && hem == 0 && col < limit)
    {
      int source_start = row * CONSOLE_COLS + col;
      int source_end = row * CONSOLE_COLS + limit;
      int destination_start = row * CONSOLE_COLS + col + 1;
      memmove (cells + destination_start,
	       cells + source_start, source_end - source_start);
    }
  else if (irm == 1 && hem == 1 && col > home)
    {
      int source_start = row * CONSOLE_COLS + home + 1;
      int source_end = row * CONSOLE_COLS + col + 1;
      int destination_start = row * CONSOLE_COLS + home;
      memmove (cells + destination_start,
	       cells + source_start, source_end - source_start);
    }

  cells[row * CONSOLE_COLS + col] = COMPOSE (rendition, codepoint);
  if (hem == 0)
    {
      col++;
      if (col > limit)
	col = limit;
    }
  else
    {
      col--;
      if (col < home)
	col = home;
    }
}

LOCAL void *
get_glyph (int glyph_set, int codepoint, int *bpd,	/* bits per dot: 1, 2, 3, 4 */
	   int *stride,		/* width of one font row in bytes */
	   int *width,		/* width of the glyph in bits */
	   int *height,		/* height of the glyph in bits */
	   int *xoffset,	/* top, left-hand corner X-offset in pixels */
	   int *yoffset		/* top left-hand corner Y-offset in pixels */
  )
{
  int i = 0;
  int maxwidth, maxheight, count;
  int bits;

  /* FIXME: do a more efficient search */
  if (glyph_set == 0)
    {
      maxwidth = FIXED8x12_MAXWIDTH;
      maxheight = FIXED8x12_MAXHEIGHT;
      count = FIXED8x12_COUNT;
      bits = 1;

      while (i < count)
	{
	  if (fixed8x12_glyphs[i].encoding == codepoint)
	    break;
	  i++;
	}
      if (i == count)
	return NULL;

      *bpd = bits;
      *stride = fixed8x12_glyphs[i].data.stride;
      *width = fixed8x12_glyphs[i].data.width;
      *height = fixed8x12_glyphs[i].data.height;
      *xoffset = fixed8x12_glyphs[i].data.xoffset;
      *yoffset = fixed8x12_glyphs[i].data.yoffset;
      return fixed8x12_glyphs[i].bitmap;
    }

  return NULL;
}

cairo_surface_t *
console_render_to_cairo_surface ()
{
  uint32_t timer, fast_blink_on, slow_blink_on;
  int width, height;
  cairo_surface_t *surf;
  uint32_t *data;
  int stride;

  int tile_j, tile_i, delta_tile_i, delta_tile_j;
  int map_index;
  uint32_t index, c;
  int glyph_bpd, glyph_stride, glyph_width, glyph_height, glyph_xoffset,
    glyph_yoffset;
  uint8_t *glyph_bitmap;

  timer = SDL_GetTicks ();
  fast_blink_on = (timer / FAST_BLINK_TIME) % 2;
  slow_blink_on = (timer / SLOW_BLINK_TIME) % 2;

  width = CONSOLE_COLS;
  height = CONSOLE_ROWS;
  surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32,
				      width * FIXED8x12_MAXWIDTH,
				      height * FIXED8x12_MAXHEIGHT);
  data = xcairo_image_surface_get_argb32_data (surf);
  stride = xcairo_image_surface_get_argb32_stride (surf);
  xcairo_surface_flush (surf);

  for (int r = 0; r < height; r++)
    {
      for (int c = 0; c < width; c++)
	{
	  uint32_t cell = cells[r * CONSOLE_COLS + c];
	  uint16_t rendering = RENDERING (cell);
	  uint16_t codepoint = CODEPOINT (cell);
	  uint16_t fg_color_index =
	    (rendering & COLOR_FG_MASK) >> COLOR_FG_OFFSET;
	  uint16_t bg_color_index =
	    (rendering & COLOR_BG_MASK) >> COLOR_BG_OFFSET;
	  uint16_t intensity_index =
	    (rendering & INTENSITY_MASK) >> INTENSITY_OFFSET;
	  uint16_t polarity = rendering & POLARITY_MASK;
	  uint16_t blink = rendering & BLINK_MASK;
	  uint16_t underline = rendering & UNDERLINE_MASK;

	  uint32_t fg_argb =
	    fg_palette[fg_color_index * CONSOLE_NUM_INTENSITIES +
		       intensity_index];
	  uint32_t bg_argb = bg_palette[bg_color_index];

	  // Xor the inverse and blink states to see if we're inverse now.
	  if ((polarity == POLARITY_NEGATIVE) !=
	      (((blink == BLINK_FAST) && (fast_blink_on))
	       || (blink == BLINK_SLOW) && (slow_blink_on)))
	    {
	      uint32_t temp_argb = fg_argb;
	      fg_argb = bg_argb;
	      bg_argb = temp_argb;
	    }

	  /* Swap again if we're the cursor and the cursor is visible */
	  if (r == row && c == col && cursor_visible)
	    {
	      uint32_t temp_argb = fg_argb;
	      fg_argb = bg_argb;
	      bg_argb = temp_argb;
	    }

	  glyph_bitmap = get_glyph (0,
				    codepoint,
				    &glyph_bpd, &glyph_stride,
				    &glyph_width, &glyph_height,
				    &glyph_xoffset, &glyph_yoffset);

	  for (int j = 0; j < glyph_height; j++)
	    {
	      for (int i = 0; i < glyph_width; i++)
		{
		  uint32_t pixel_argb;

		  if ((underline == UNDERLINE_SINGLY
		       || underline == UNDERLINE_DOUBLY)
		      && (j == glyph_height - 1))
		    pixel_argb = fg_argb;
		  else if ((underline == UNDERLINE_DOUBLY) &&
			   (j == glyph_height - 2))
		    pixel_argb = fg_argb;
		  else if (glyph_bitmap
			   && glyph_bitmap[j] & (1 << (glyph_width - i - 1)))
		    pixel_argb = fg_argb;
		  else
		    pixel_argb = bg_argb;

		  data[(r * FIXED8x12_MAXHEIGHT + j) * stride
		       + c * FIXED8x12_MAXWIDTH + i] = pixel_argb;
		}
	    }
	  xcairo_surface_mark_dirty (surf);
	}
    }
  return surf;
}

void
console_write_latin1_string (uint8_t * str)
{
  int i;
  for (int i = 0; i < strlen (str); i++)
    console_write_char (str[i], 0, 0, 0, CONSOLE_COLS - 1);
}

void
console_test_pattern (void)
{
  for (int i = 0; i < FIXED8x12_COUNT; i++)
    {
      console_write_char (fixed8x12_glyphs[i].encoding, 0, 0, 0, 59);
      if (col == CONSOLE_COLS - 1)
	{
	  row++;
	  col = 0;
	}
    }

  row++;
  col = 0;
  console_set_bgcolor (COLOR_BG_DEFAULT);
  console_write_latin1_string ("DEFAULT");
  console_set_bgcolor (COLOR_BG_BLACK);
  console_write_latin1_string ("BLACK");
  console_set_bgcolor (COLOR_BG_RED);
  console_write_latin1_string ("RED");
  console_set_bgcolor (COLOR_BG_GREEN);
  console_write_latin1_string ("GREEN");
  console_set_bgcolor (COLOR_BG_YELLOW);
  console_write_latin1_string ("YELLOW");
  console_set_bgcolor (COLOR_BG_BLUE);
  console_write_latin1_string ("BLUE");
  console_set_bgcolor (COLOR_BG_MAGENTA);
  console_write_latin1_string ("MAGENTA");
  console_set_bgcolor (COLOR_BG_CYAN);
  console_write_latin1_string ("CYAN");
  console_set_bgcolor (COLOR_BG_WHITE);
  console_write_latin1_string ("WHITE");
  console_set_bgcolor (COLOR_BG_TRANSPARENT);
  console_write_latin1_string ("CLEAR");

  for (int i = 0; i < 3; i++)
    {
      row++;
      col = 0;
      console_set_fgcolor (COLOR_FG_DEFAULT);
      if (i == 0)
	{
	  console_set_intensity (INTENSITY_BOLD);
	  console_write_latin1_string ("BOLD   ");
	}
      else if (i == 1)
	{
	  console_set_intensity (INTENSITY_FAINT);
	  console_write_latin1_string ("FAINT  ");
	}
      else if (i == 2)
	{
	  console_set_intensity (INTENSITY_NORMAL);
	  console_write_latin1_string ("NORMAL ");
	}
      console_set_fgcolor (COLOR_FG_BLACK);
      console_write_latin1_string ("BLACK");
      console_set_fgcolor (COLOR_FG_RED);
      console_write_latin1_string ("RED");
      console_set_fgcolor (COLOR_FG_GREEN);
      console_write_latin1_string ("GREEN");
      console_set_fgcolor (COLOR_FG_YELLOW);
      console_write_latin1_string ("YELLOW");
      console_set_fgcolor (COLOR_FG_BLUE);
      console_write_latin1_string ("BLUE");
      console_set_fgcolor (COLOR_FG_MAGENTA);
      console_write_latin1_string ("MAGENTA");
      console_set_fgcolor (COLOR_FG_CYAN);
      console_write_latin1_string ("CYAN");
      console_set_fgcolor (COLOR_FG_WHITE);
      console_write_latin1_string ("WHITE");
      console_set_fgcolor (COLOR_FG_TRANSPARENT);
      console_write_latin1_string ("CLEAR");
    }

  row++;
  col = 0;
  console_set_fgcolor (COLOR_FG_DEFAULT);
  console_set_bgcolor (COLOR_BG_DEFAULT);
  console_set_blink (BLINK_NONE);
  console_write_latin1_string ("STEADY");
  console_set_blink (BLINK_SLOW);
  console_write_latin1_string ("BLINK1");
  console_set_blink (BLINK_FAST);
  console_write_latin1_string ("BLINK2");
  console_set_blink (BLINK_NONE);

  console_set_underline (UNDERLINE_NONE);
  console_write_latin1_string ("NORMAL");
  console_set_underline (UNDERLINE_SINGLY);
  console_write_latin1_string ("UNDERLINE1");
  console_set_underline (UNDERLINE_DOUBLY);
  console_write_latin1_string ("UNDERLINE2");
  console_set_underline (UNDERLINE_NONE);

  row++;
  col = 0;
  console_set_polarity (POLARITY_POSITIVE);
  console_write_latin1_string ("POSITIVE");
  console_set_polarity (POLARITY_NEGATIVE);
  console_write_latin1_string ("NEGATIVE");
  console_set_polarity (POLARITY_POSITIVE);

  row++;
  col = 0;
  console_write_latin1_string ("DELETE LEFT");
  console_delete_left (1);
}
