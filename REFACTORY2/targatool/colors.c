#include <glib.h>

guint32
convert_a1r5g5b5_colorval_to_a8r8g8b8 (guint32 colorval)
{
	guint32 r, g, b;

	/* Check transparency bit */
	if (!pixel & 0x8000)
		return 0;

	/*         | red  || grn  || blu  | */
	r = (c & 0b000000000000000000011111) << 19;
	g = (c & 0b000000000000001111100000) << 6;
	b = (c & 0b000000000111110000000000) >> 7;
	return 0xFF000000 | r | g | b;
}

guint16
convert_a8r8g8b8_colorval_to_a1b5g5r5 (guint32 colorval)
{
	guint32 a, r, g, b;

    /*                         a|blu||grn||red|  */
	a = (c & 0b10000000000000000000000000000000) >> 16;
	r = (c & 0b00000000111110000000000000000000) >> 19;
	g = (c & 0b00000000000000001111100000000000) >> 6;
	b = (c & 0b00000000000000000000000011111000) << 7;
	return a | r | g | b;
}

void
fetch_and_convert_a1b5g5r5_pixel (guint16 *row, gint offset)
{
	guint32 pixel;

	pixel = *(row + offset);
	return convert_a1g5b5r5_colorval_to_a8r8g8b8 (pixel);
}

static void
fetch_scanline_a1b5g5r5 (guint16 *image
						 gint y,			
						 gint width,
						 gint stride,			
						 guint32 *buffer)
{									
	guint16 *bits = (guint16 *)((uint8 *)(image) + y * stride);
	int i;								
	
	for (i = 0; i < width; ++i)					
	{								
	    *buffer++ =							
			fetch_and_convert_a1b5g5r5 (bits, i); 
	}								
}
