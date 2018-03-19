// targa.c -- Functions related to the parsing of Targa files

#include <glib.h>
#include <glib/gprintf.h>
#include <string.h>
#include "targa.h"
#include "common.h"
#include "io.h"


#define MEM0(dest) \
	memset((void *)(dest),0,sizeof(dest))

// The error number of the last error
targa_error_t targa_error = TARGA_OK;

/* This is a paranoid parser for Targa files. It should fail on most
 * errors. */
targa_error_t targa_parse_stream (GInputStream *istream, targa_image_t *t)
{
	int i;
	guint filesize;
	guint8 raw_footer[TARGA_PACKED_FOOTER_LEN];
	targa_error_t error;
	size_t id_size = 0;
	size_t color_map_size = 0;
	size_t image_unpacked_size = 0;

	t->version = 1;
	t->extended_info = 0;

	if (!g_seekable_seek (G_SEEKABLE(istream), 0, G_SEEK_END, NULL, NULL))
	{
		error = TARGA_READ_ERROR;
		g_printerr ("read error");
		goto cleanup;
	}
	filesize = g_seekable_tell (G_SEEKABLE(istream)) + 1;
	g_seekable_seek (G_SEEKABLE(istream), 0, G_SEEK_SET, NULL, NULL);

	MEM0 (&(t->header));
	MEM0 (&(t->extension));
	MEM0 (&(t->footer));
	MEM0 (&(t->data));

	/* Since it is impossible to validate the length of the file
	 * without parsing most of the file, we'll end up checking its
	 * size many different times as we learn more about the file.
	 */

	/* First, we see if it at least contains a header. */
	if (filesize < TARGA_PACKED_HEADER_LEN)
	{
		error = TARGA_HEADER_TOO_SHORT;
		g_printerr ("the file ends prematurely (header)");
		goto cleanup;
	}

	// Then we check to see if this is a Targa file v2.0
	if (filesize < TARGA_PACKED_HEADER_LEN + TARGA_PACKED_FOOTER_LEN)
	{
		t->version = 1;
	}
	else
	{
		g_seekable_seek (G_SEEKABLE(istream), - TARGA_PACKED_FOOTER_LEN, G_SEEK_END, NULL, NULL);
		sread_guint8_array (raw_footer, TARGA_PACKED_FOOTER_LEN, istream);
		if (memcmp((void *) &raw_footer[TARGA_PACKED_FOOTER_STRING_START], "TRUEVISION-XFILE", TARGA_PACKED_FOOTER_STRING_LEN) == 0)
			t->version = 2;
		else
			t->version = 1;
	}

	// If it is a 2.0 file, we can properly parse the footer
	if (t->version == 2)
	{
		// Field 28 - Extension Area Offset
		t->footer.extension_offset = raw_footer[0] | (raw_footer[1] << 8) | (raw_footer[2] << 16) | (raw_footer[3] << 24);

		// Field 29 - Developer Directory Offset
		t->footer.developer_area_offset = raw_footer[4] | (raw_footer[5] << 8) | (raw_footer[6] << 16) | (raw_footer[7] << 24);

		// Field 30 - Signature
		memcpy((void *) t->footer.signature, "TRUEVISION-XFILE", 16);

		// Field 31 - Reserved Character
		t->footer.signature[16] = '.';

		// Field 32 - Binary Zero String Terminator
		t->footer.signature[17] = '\0';
	}
	else
	{
		t->footer.extension_offset = 0;
		t->footer.developer_area_offset = 0;
		MEM0 (t->footer.signature);
	}

	if (t->footer.extension_offset)
		t->extended_info = 1;

	/* Now we know if it has a header, a footer, an extension, and a
	 * developer area.  We've done the footer.  Now let's do the
	 * rest. */

	g_seekable_seek (G_SEEKABLE(istream), 0, G_SEEK_SET, NULL, NULL);

	// Field 1 - ID Length
	t->header.id_string_length = sread_guint8 (istream);

	// Field 2 - Color Map Type
	t->header.color_map_type = sread_guint8 (istream);
	if (!TARGA_COLOR_MAP_TYPE_VALID(t))
	{
		error = TARGA_COLOR_MAP_TYPE_OUT_OF_RANGE;
		g_printerr ("unknown color map type: %u", (unsigned int) t->header.color_map_type);
		goto cleanup;
	}

	// Field 3 - Image Type
	t->header.image_type = (targa_image_type_t) sread_guint8 (istream);
	if (!TARGA_IMAGE_TYPE_VALID(t))
	{
		error = TARGA_IMAGE_TYPE_OUT_OF_RANGE;
		g_printerr ("unknown image type: %u", (unsigned int) t->header.image_type);
		goto cleanup;
	}
	if (t->header.color_map_type == TARGA_COLOR_MAP_TYPE_NO_COLOR_MAP
		&& TARGA_IMAGE_TYPE_COLOR_MAPPED(t))
	{
		error = TARGA_MISSING_COLOR_MAP;
		g_printerr ("image type %u requires a color map", (unsigned int) t->header.image_type);
		goto cleanup;
	}
	if (t->header.color_map_type == TARGA_COLOR_MAP_TYPE_COLOR_MAP
		&& !TARGA_IMAGE_TYPE_COLOR_MAPPED(t))
	{
		error = TARGA_UNNECESSARY_COLOR_MAP;
		g_printerr ("image type %u should not have a color map", (unsigned int) t->header.image_type);
		goto cleanup;
	}

	// Field 4 - Color Map Specification

	// Field 4.1 - First Entry Index
	t->header.color_map_first_entry_index = sread_guint16_LE (istream);
	if (t->header.color_map_first_entry_index != 0
		&& t->header.color_map_type == TARGA_COLOR_MAP_TYPE_NO_COLOR_MAP)
	{
		error = TARGA_COLOR_MAP_FIRST_ENTRY_INDEX_INVALID;
		g_printerr ("color map first entry index %u must be zero when there is no color map",
				 (unsigned int) t->header.color_map_first_entry_index);
		goto cleanup;
	}

	// Field 4.2 - Color Map Length
	t->header.color_map_length = sread_guint16_LE (istream);
	if (t->header.color_map_type == TARGA_COLOR_MAP_TYPE_NO_COLOR_MAP
		&& t->header.color_map_length > 0)
	{
		error = TARGA_COLOR_MAP_LENGTH_INVALID;
		g_printerr ("color map length %u must be zero when there is no color map",
				 (unsigned int) t->header.color_map_length);
		goto cleanup;
	}

	// Field 4.3 - Color Map Entry Size
	if (t->header.color_map_type == TARGA_COLOR_MAP_TYPE_COLOR_MAP
		&& t->header.color_map_length == 0)
	{
		error = TARGA_COLOR_MAP_LENGTH_INVALID;
		g_printerr ("color map length %u must be greater than zero when there is a color map",
				 (unsigned int) t->header.color_map_length);
		goto cleanup;
	}
	if (t->header.color_map_first_entry_index > t->header.color_map_length)
	{
		error = TARGA_COLOR_MAP_FIRST_ENTRY_INDEX_OUT_OF_RANGE;
		g_printerr ("color map first entry index %u must be less than the color map length %u",
				 (unsigned int) t->header.color_map_first_entry_index, (unsigned int) t->header.color_map_length);
		goto cleanup;
	}
	t->header.color_map_entry_size = sread_guint8 (istream);
	if (t->header.color_map_type == TARGA_COLOR_MAP_TYPE_NO_COLOR_MAP
		&& t->header.color_map_entry_size != 0)
	{
		error = TARGA_COLOR_MAP_ENTRY_SIZE_INVALID;
		g_printerr ("color map width %u must be zero when there is no color map",
				 (unsigned int) t->header.color_map_entry_size);
		goto cleanup;
	}
	if (t->header.color_map_type == TARGA_COLOR_MAP_TYPE_COLOR_MAP
		&& !TARGA_COLOR_MAP_ENTRY_SIZE_VALID(t))
	{
		error = TARGA_COLOR_MAP_ENTRY_SIZE_OUT_OF_RANGE;
		g_printerr ("color map width %u is not one of the commonly allowed values",
				 (unsigned int) t->header.color_map_entry_size);
		goto cleanup;
	}

	// 15bpp is a perfectly valid color map width, but, I don't want to 
	// support it.
	if (t->header.color_map_entry_size == 15)
		t->header.color_map_entry_size = 16;

	// Field 5 - Image Specification
	// Field 5.1 - X Origin of Image
	t->header.x_origin = sread_guint16_LE (istream);

	// Field 5.2 - Y Origin of Image
	t->header.y_origin = sread_guint16_LE (istream);

	// Field 5.3 - Image Width
	t->header.image_width = sread_guint16_LE (istream);

	// Field 5.4 - Image Height
	t->header.image_height = sread_guint16_LE (istream);

	// Field 5.5 - Pixel Depth
	t->header.pixel_depth = sread_guint8 (istream);
	if (!(TARGA_PIXEL_DEPTH_VALID(t) || t->header.color_map_type == TARGA_COLOR_MAP_TYPE_COLOR_MAP))
	{
		error = TARGA_PIXEL_DEPTH_OUT_OF_RANGE;
		g_printerr ("pixel depth %u is not one of the commonly allowed values", (unsigned int) t->header.pixel_depth);
		goto cleanup;
	}
	// 15bpp is a perfectly valid pixel depth, but, I don't want to 
	// support it.
	if (t->header.pixel_depth == 15)
		t->header.pixel_depth = 16;

	// Field 5.6 - Image Descriptor
	t->header.image_descriptor = sread_guint8 (istream);
	if (t->header.image_descriptor & 0xC0)
	{
		error = TARGA_IMAGE_DESCRIPTOR_OUT_OF_RANGE;
		g_printerr ("image descriptor %u is out of range", (unsigned int) t->header.image_descriptor);
		goto cleanup;
	}

	// Field 6 - Image ID (Variable)
	id_size = t->header.id_string_length;
	if (filesize < TARGA_PACKED_HEADER_LEN + id_size)
	{
		error = TARGA_ID_STRING_TOO_SHORT;
		g_printerr ("the file ends prematurely (ID string)");
		goto cleanup;
	}

	if (id_size == 0)
		t->data.id_string = (char *) 0;
	else
	{
		t->data.id_string = g_new0 (gchar, id_size + 1);
		sread_gchar_array (t->data.id_string, id_size, istream);
	}

	// Field 7 - Color Map Data (variable)
	if (t->header.color_map_type == TARGA_COLOR_MAP_TYPE_COLOR_MAP)
		color_map_size = (size_t) t->header.color_map_length
			* (size_t) BIT_TO_BYTE(t->header.color_map_entry_size);
	else
		color_map_size = 0;

	if (filesize < TARGA_PACKED_HEADER_LEN + id_size + color_map_size)
	{
		error = TARGA_COLOR_MAP_TOO_SHORT;
		g_printerr ("the file ends prematurely (color map)");
		goto cleanup;
	}

	if (color_map_size > 0)
	{
		t->data.color_map_data = g_new0 (guint8, color_map_size);
		sread_guint8_array (t->data.color_map_data, color_map_size, istream);
	}
	else
		t->data.color_map_data = (guint8 *) 0;

	// Field 8 - Image Data
	if (t->header.image_type == TARGA_IMAGE_TYPE_NO_IMAGE_DATA)
	{
		image_unpacked_size = 0;
		t->data.image_data = (guint8 *) 0;
	}
	else
	{
		image_unpacked_size = TARGA_IMAGE_SIZE(t) * BIT_TO_BYTE (t->header.pixel_depth);
		t->data.image_data = g_new0 (guint8, image_unpacked_size);

		// How to read the unpack the data depends on the format
		if (TARGA_IMAGE_TYPE_UNCOMPRESSED(t))
		{
			// For uncompressed data, we can sanity check the file size
			if (filesize < TARGA_PACKED_HEADER_LEN + id_size + color_map_size + image_unpacked_size)
			{
				error = TARGA_IMAGE_DATA_TOO_SHORT;
				g_printerr ("the file ends prematurely (image data)");
			}
			sread_guint8_array (t->data.image_data, image_unpacked_size, istream);
		}
		else if (TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED(t))
		{
			int j;

			/* FIXME: I'm uncompressing the data here.  I should really
			 * make RLE compress and RLE uncompress into methods.
			 */

			// For RLE data, we can't sanity check the file size first.
			for (j = 0; j < t->header.image_height; j ++)
			{
				gint count;
				count = sread_rle_array (&(t->data.image_data[j * t->header.image_width * BIT_TO_BYTE (t->header.pixel_depth)]),
										BIT_TO_BYTE (t->header.pixel_depth),
										t->header.image_width, 
										istream);
				if (count == -1)
				{
					error = TARGA_IMAGE_DATA_TOO_SHORT;
					g_printerr ("the file ends prematurely (RLE image data)");
					goto cleanup;
				}
				image_unpacked_size += count;
			}
			/* Since we've uncompressed the data... */
			if (t->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED)
				t->header.image_type = TARGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED;
			else if (t->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR)
				t->header.image_type = TARGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR;
			else if (t->header.image_type == TARGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)
				t->header.image_type = TARGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE;
		}
	}

	// Field 9 - Developer Data (variable)
	// This is vendor specific, so we ignore it.

	if (t->footer.extension_offset)
	{
		if (filesize < t->footer.extension_offset + TARGA_EXTENSION_AREA_LEN + TARGA_PACKED_FOOTER_LEN)
		{
			error = TARGA_EXTENSION_AREA_TOO_SHORT;
			g_printerr ("the file ends prematurely (extension area)");
			goto cleanup;
		}
		if (!g_seekable_seek (G_SEEKABLE(istream), (long) t->footer.extension_offset, G_SEEK_SET, NULL, NULL))
		{
			error = TARGA_READ_ERROR;
			g_printerr ("read error");
			goto cleanup;
		}

		// Field 10 - Extension Size (2 Bytes)
		t->extension.extension_size = sread_guint16_LE (istream);
		if (t->extension.extension_size != TARGA_EXTENSION_SIZE)
		{
			error = TARGA_EXTENSION_SIZE_OUT_OF_RANGE;
			g_printerr ("extension size %u is out of range (must equal %u)", (guint) t->extension.extension_size,
					   (guint) TARGA_EXTENSION_SIZE);
			goto cleanup;
		}

		// Field 11 - Author Name (41 Bytes)
		sread_gchar_array (t->extension.author_name, TARGA_AUTHOR_NAME_LEN, istream);
		if (t->extension.author_name[TARGA_AUTHOR_NAME_LEN-1] != '\0')
		{
			error = TARGA_AUTHOR_NAME_NOT_NULL_TERMINATED;
			g_printerr ("the author name is not null terminated");
			goto cleanup;
		}

		// Field 12 - Author Comments (324 Bytes)
		for (i = 0; i < TARGA_AUTHOR_COMMENT_LINES_NUM; i ++)
		{
			sread_gchar_array ( t->extension.author_comment[i], TARGA_AUTHOR_COMMENT_LINE_LEN, istream);
			if (t->extension.author_comment[i][TARGA_AUTHOR_COMMENT_LINE_LEN-1] != '\0')
			{
				error = TARGA_AUTHOR_COMMENT_NOT_NULL_TERMINATED;
				g_printerr ("author comment line %u is not null terminated", (unsigned int) i);
				goto cleanup;
			}
		}

		// Field 13 - Date/Time Stamp (12 Bytes)
		t->extension.month = sread_guint16_LE (istream);
		if (t->extension.month < 1 || t->extension.month > 12)
		{
			error = TARGA_MONTH_OUT_OF_RANGE;
			goto cleanup;
		}
		t->extension.day = sread_guint16_LE (istream);
		if (t->extension.day < 1 || t->extension.day > 31)
		{
			error = TARGA_DAY_OUT_OF_RANGE;
			goto cleanup;
		}
		t->extension.year = sread_guint16_LE (istream);
		t->extension.hour = sread_guint16_LE (istream);
		if (t->extension.hour > 23)
		{
			error = TARGA_HOUR_OUT_OF_RANGE;
			goto cleanup;
		}
		t->extension.minute = sread_guint16_LE (istream);
		if (t->extension.minute > 59)
		{
			error = TARGA_MINUTE_OUT_OF_RANGE;
			goto cleanup;
		}
		t->extension.second = sread_guint16_LE (istream);
		if (t->extension.second > 59)
		{
			error = TARGA_SECOND_OUT_OF_RANGE;
			goto cleanup;
		}

		// Field 14 - Job Name/ID (41 Bytes)
		sread_gchar_array (t->extension.job_id, TARGA_JOB_ID_LEN, istream);
		if (t->extension.job_id[TARGA_JOB_ID_LEN-1] != '\0')
		{
			error = TARGA_JOB_ID_NOT_NULL_TERMINATED;
			goto cleanup;
		}

		// Field 15 - Job Time (6 Bytes)

		t->extension.job_hour = sread_guint16_LE (istream);
		t->extension.job_minute = sread_guint16_LE (istream);
		if (t->extension.job_minute > 59)
		{
			error = TARGA_JOB_MINUTE_OUT_OF_RANGE;
			goto cleanup;
		}
		t->extension.job_second = sread_guint16_LE(istream);
		if (t->extension.job_second > 59)
		{
			error = TARGA_JOB_SECOND_OUT_OF_RANGE;
			goto cleanup;
		}

		// Field 16 - Software ID (41 Bytes)
		sread_gchar_array (t->extension.software_id, TARGA_SOFTWARE_ID_LEN, istream);
		if (t->extension.software_id[TARGA_SOFTWARE_ID_LEN-1] != '\0')
		{
			error = TARGA_SOFTWARE_ID_NOT_NULL_TERMINATED;
			goto cleanup;
		}

		// Field 17 - Software Version (3 Bytes)
		t->extension.version_number = sread_guint16_LE (istream);
		t->extension.version_letter = (char) sread_guint8 (istream);
		// FIXME should test that version letter is a letter or space here

		// Field 18 - Key Color (4 Bytes)
		t->extension.key_color_argb = sread_guint32_LE (istream);

		// Field 19 - Pixel Aspect Ratio (4 Bytes)
		t->extension.pixel_aspect_ratio_numerator = sread_guint16_LE (istream);
		t->extension.pixel_aspect_ratio_denominator = sread_guint16_LE (istream);

		// Field 20 - Gamma Value (4 Bytes)
		t->extension.gamma_correction_factor_numerator = sread_guint16_LE (istream);
		t->extension.gamma_correction_factor_denominator = sread_guint16_LE (istream);
		if (t->extension.gamma_correction_factor_denominator != 0)
		{
			double gamma_correction = ((double) t->extension.gamma_correction_factor_numerator
									   / (double) t->extension.gamma_correction_factor_denominator);
			if (gamma_correction > 10.0)
			{
				error = TARGA_GAMMA_CORRECTION_OUT_OF_RANGE;
				goto cleanup;
			}
		}

		// Field 21 - Color Correction Offset (4 Bytes)
		t->extension.color_correction_offset = sread_guint32_LE (istream);

		// Field 22 - Postage Stamp Offset (4 Bytes)
		t->extension.stamp_offset = sread_guint32_LE (istream);

		// Field 23 - Scan Line Offset (4 Bytes)
		t->extension.scan_line_offset = sread_guint32_LE (istream);

		// Field 24 - Attributes Type (1 Byte)
		t->extension.alpha_attribute = sread_guint8 (istream);
		if (!TARGA_ALPHA_ATTRIBUTES_VALID(t))
		{
			error = TARGA_ALPHA_ATTRIBUTES_OUT_OF_RANGE;
			g_printerr ("alpha attributes %u out of range", (unsigned int) t->extension.alpha_attribute);
			goto cleanup;
		}
		if (t->extension.alpha_attribute == TARGA_ALPHA_ATTRIBUTES_NO_ALPHA && ((t->header.image_descriptor & 0x0F) != 0))
		{
			error = TARGA_ALPHA_ATTRIBUTES_CONFLICT;
			g_printerr ("header has no alpha attribute set but has %u alpha channel bit(s) set",
					 (unsigned int)(t->header.image_descriptor & 0x0F));
		}
		
		// Field 25 - Scan Line Table
		// We don't read the scan line table.  It exists as an optimization to let a program
		// read in only part of an image.  We always unpack the whole image.

		// Field 26 - Postage Stamp Image
		// We don't read in the postage stamp image, which is a thumbnail image, because,
		// we only care about the full image.

		// Field 27 - Color Correction Table
		// FIXME: read the color correction table
	}

	return TARGA_OK;

cleanup:
	if (t->data.id_string)
		g_free (t->data.id_string);
	if (t->data.color_map_data)
		g_free (t->data.color_map_data);
	if (t->data.image_data)
		g_free (t->data.image_data);

	return error;
}

gboolean targa_has_image (const targa_image_t *t)
{
	if (t->header.image_type != TARGA_IMAGE_TYPE_NO_IMAGE_DATA)
		return 1;
	return 0;
}

gboolean targa_has_palette (const targa_image_t *t)
{
	int test1, test2, test3;

	/* Some applications were known to define color maps for true
	 * color images so they could use the color map area for other
	 * things.  This is why the image_type is the more important
	 * indicator of whether something has a color map. */
	test1 = TARGA_IMAGE_TYPE_COLOR_MAPPED(t);
	test2 = t->header.color_map_type == TARGA_COLOR_MAP_TYPE_COLOR_MAP;
	test3 = t->header.color_map_length > 0;
	if (test1 && test2 && test3)
		return 1;
	return 0;
}

void targa_get_image_dimensions (const targa_image_t *t, guint *width, guint *height)
{
	if (targa_has_image (t))
	{
		*width = t->header.image_width;
		*height = t->header.image_height;
	}
	else
	{
		*width = 0;
		*height = 0;
		g_warning ("asked for image dimensions for a non-image Targa file");
	}
}

void targa_get_image_orientation (const targa_image_t *t, targa_hflip_t *hflip, targa_vflip_t *vflip)
{
	if (targa_has_image (t))
	{
		*hflip = t->header.image_descriptor & 0b00010000 ? 1 : 0;
		*vflip = t->header.image_descriptor & 0b00100000 ? 1 : 0;
	}
	else
	{
		*hflip = 0;
		*vflip = 0;
		g_warning ("asked for image orientation for a non-image Targa");
	}
}

color_format_t targa_get_image_color_format (const targa_image_t *t)
{
	color_format_t c;
	guint8 bpp = t->header.pixel_depth;

	if (TARGA_IMAGE_TYPE_BLACK_AND_WHITE(t))
	{
		if (bpp == 8)
			c = COLOR_g8;
		else if (bpp == 16)
			c = COLOR_g16;
		else
			c = COLOR_x;
	}
	else if (TARGA_IMAGE_TYPE_COLOR_MAPPED(t))
	{
		if (bpp == 8)
			c = COLOR_i8;
		else if (bpp == 16)
			c = COLOR_i16;
		else
			c = COLOR_x;
	}
	else if (TARGA_IMAGE_TYPE_TRUE_COLOR(t))
	{
		guint attribute = t->header.image_descriptor & 0x0f;

		switch (bpp)
		{
		case 16:
			if (attribute)
				c = COLOR_o1r5g5b5;
			else
				c = COLOR_x1r5g5b5;
			break;
		case 24:
			c = COLOR_r8g8b8;
			break;
		case 32:
			if (attribute == 8)
				c = COLOR_a8r8g8b8;
			else
				c = COLOR_x8r8g8b8;
			break;
		default:
			c = COLOR_x;
		}

	}
	else
		c = COLOR_x;

	return c;
}

void targa_set_image_color_format (targa_image_t *t,
								   color_format_t c)
{
	if (TARGA_IMAGE_TYPE_TRUE_COLOR(t))
	{
		switch (c)
		{
		case COLOR_x1r5g5b5:
			t->header.pixel_depth = 16;
			t->header.image_descriptor &= 0xf0;
			break;
		case COLOR_o1r5g5b5:
			t->header.pixel_depth = 16;
			t->header.image_descriptor = (t->header.image_descriptor & 0xf0) | 0x01;
			break;
		case COLOR_r8g8b8:
			t->header.pixel_depth = 24;
			t->header.image_descriptor &= 0xf0;
			break;
		case COLOR_x8r8g8b8:
			t->header.pixel_depth = 32;
			t->header.image_descriptor &= 0xf0;
			break;
		case COLOR_a8r8g8b8:
			t->header.pixel_depth = 32;
			t->header.image_descriptor = (t->header.image_descriptor & 0xf0) | 0x08;
			break;
		default:
			g_warning ("tried to set image to unsupported color type");
		}
	}
	else
		g_warning ("setting the color type of non-true-color images is not supported");

}


color_format_t targa_get_palette_color_format (const targa_image_t *t)
{
	color_format_t c;

	if (TARGA_IMAGE_TYPE_COLOR_MAPPED(t))
	{
		guint8 bpp = t->header.color_map_entry_size;
		guint attribute = t->header.image_descriptor & 0x0f;

		switch (bpp)
		{
		case 16:
			if (attribute)
				c = COLOR_o1r5g5b5;
			else
				c = COLOR_x1r5g5b5;
			break;
		case 24:
			c = COLOR_b8g8r8;
			break;
		case 32:
			if (attribute == 8)
				c = COLOR_a8r8g8b8;
			else
				c = COLOR_x8r8g8b8;
			break;
		default:
			c = COLOR_x;
		}
	}
	else
		c = COLOR_x;

	return c;
}

void targa_set_palette_color_format (targa_image_t *t,
									 color_format_t c)
{
	if (TARGA_IMAGE_TYPE_COLOR_MAPPED(t))
	{
		switch (c)
		{
		case COLOR_x1r5g5b5:
			t->header.color_map_entry_size = 16;
			t->header.image_descriptor &= 0xf0;
			break;
		case COLOR_o1r5g5b5:
			t->header.color_map_entry_size = 16;
			t->header.image_descriptor = (t->header.image_descriptor & 0xf0) | 0x01;
			break;
		case COLOR_r8g8b8:
			t->header.color_map_entry_size = 24;
			t->header.image_descriptor &= 0xf0;
			break;
		case COLOR_x8r8g8b8:
			t->header.color_map_entry_size = 32;
			t->header.image_descriptor &= 0xf0;
			break;
		case COLOR_a8r8g8b8:
			t->header.color_map_entry_size = 32;
			t->header.image_descriptor = (t->header.image_descriptor & 0xf0) | 0x08;
			break;
		default:
			g_warning ("tried to set palette to unsupported color type");
		}
	}
	else
		g_warning ("tried to set palette color type of non-color-mapped image");

}


guint targa_get_color_map_size (const targa_image_t *t)
{
	return t->header.color_map_length;
}

guint targa_get_color_map_first_index (const targa_image_t *t)
{
	return t->header.color_map_first_entry_index;
}


targa_error_t targa_write (GOutputStream *ostream, targa_image_t *t)
{
	t->header.y_origin = 0;
	t->header.image_descriptor &= 0x0f;

	swrite_guint8 (t->header.id_string_length, ostream);
	swrite_guint8 (t->header.color_map_type, ostream);
	swrite_guint8 (t->header.image_type, ostream);
	swrite_guint16_LE (t->header.color_map_first_entry_index, ostream);
	swrite_guint16_LE (t->header.color_map_length, ostream);
	swrite_guint8 (t->header.color_map_entry_size, ostream);
	swrite_guint16_LE (t->header.x_origin, ostream);
	swrite_guint16_LE (t->header.y_origin, ostream);
	swrite_guint16_LE (t->header.image_width, ostream);
	swrite_guint16_LE (t->header.image_height, ostream);
	swrite_guint8 (t->header.pixel_depth, ostream);
	swrite_guint8 (t->header.image_descriptor, ostream);
	if (t->header.id_string_length)
		swrite_gchar_array (t->data.id_string, t->header.id_string_length, ostream);
	if (t->header.color_map_length)
		swrite_guint8_array (t->data.color_map_data,
							 (int) t->header.color_map_length * BIT_TO_BYTE(t->header.color_map_entry_size),
							 ostream);
	if (t->header.image_width > 0 && t->header.image_height > 0)
		swrite_guint8_array (t->data.image_data,
							 TARGA_IMAGE_SIZE(t) * BIT_TO_BYTE(t->header.pixel_depth),
							 ostream);
	return TARGA_OK;
}

targa_error_t map_parse_stream (GInputStream *istream, targa_image_t *t)
{
	gchar *text;
	guint filesize, bytes_read;
	targa_error_t error;
	size_t image_unpacked_size = 0;
	GScanner *scanner;       

	t->version = 1;
	t->extended_info = 0;

	/* The 1st line contains x dimension and y dimension */
gboolean            g_input_stream_read_all             (GInputStream *stream,
                                                         void *buffer,
                                                         gsize count,
                                                         gsize *bytes_read,
                                                         GCancellable *cancellable,
                                                         GError **error);

	if (!g_seekable_seek (G_SEEKABLE(istream), 0, G_SEEK_END, NULL, NULL))
	{
		error = TARGA_READ_ERROR;
		g_printerr ("read error");
		goto cleanup;
	}
	filesize = g_seekable_tell (G_SEEKABLE(istream)) + 1;
	g_seekable_seek (G_SEEKABLE(istream), 0, G_SEEK_SET, NULL, NULL);

	MEM0 (&(t->header));
	MEM0 (&(t->extension));
	MEM0 (&(t->footer));
	MEM0 (&(t->data));

	text = g_new0 (gchar, filesize + 1);

	g_input_stream_read_all (istream, text, filesize, &bytes_read, NULL, NULL);
	scanner = g_scanner_new (NULL);
	g_scanner_input_text (scanner, text, filesize);
	

	/* Since it is impossible to validate the length of the file
	 * without parsing most of the file, we'll end up checking its
	 * size many different times as we learn more about the file.
	 */

	/* First, we see if it at least contains a header. */
	if (filesize < TARGA_PACKED_HEADER_LEN)
	{
		error = TARGA_HEADER_TOO_SHORT;
		g_printerr ("the file ends prematurely (header)");
		goto cleanup;
	}

	// Field 28 - Extension Area Offset
	t->footer.extension_offset = 0;

	// Field 29 - Developer Directory Offset
	t->footer.developer_area_offset = 0;

	// Field 30 - Signature
	memcpy((void *) t->footer.signature, "TRUEVISION-XFILE", 16);
	
	// Field 31 - Reserved Character
	t->footer.signature[16] = '.';
	
	// Field 32 - Binary Zero String Terminator
	t->footer.signature[17] = '\0';

	t->extended_info = 0;

	// Field 1 - ID Length
	t->header.id_string_length = 0;

	// Field 2 - Color Map Type
	t->header.color_map_type = TARGA_COLOR_MAP_TYPE_NO_COLOR_MAP;

	// Field 3 - Image Type
	t->header.image_type = TARGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE;

	// Field 4 - Color Map Specification

	// Field 4.1 - First Entry Index
	t->header.color_map_first_entry_index = 0;;

	// Field 4.2 - Color Map Length
	t->header.color_map_length = 0;

	// Field 4.3 - Color Map Entry Size
	t->header.color_map_entry_size = 0;

	// Field 5 - Image Specification
	// Field 5.1 - X Origin of Image
	t->header.x_origin = 0;

	// Field 5.2 - Y Origin of Image
	t->header.y_origin = 0;

	// Field 5.3 - Image Width
	g_scanner_get_next_token (scanner);
	t->header.image_width = g_scanner_cur_value (scanner).v_int;

	// Field 5.4 - Image Height
	g_scanner_get_next_token (scanner);
	t->header.image_height = g_scanner_cur_value (scanner).v_int;

	// Field 5.5 - Pixel Depth
	t->header.pixel_depth = 16;

	// Field 5.6 - Image Descriptor
	t->header.image_descriptor = 0;

	// Field 6 - Image ID (Variable)
	t->data.id_string = (char *) 0;

	// Field 7 - Color Map Data (variable)
	t->data.color_map_data = (guint8 *) 0;

	// Field 8 - Image Data
	image_unpacked_size = TARGA_IMAGE_SIZE(t) * BIT_TO_BYTE (t->header.pixel_depth);
	t->data.image_data = g_new0 (guint8, image_unpacked_size);
	for (int i = 0; i < TARGA_IMAGE_SIZE (t); i ++)
	  {
	    g_scanner_get_next_token (scanner);
	    ((guint16 *) (t->data.image_data))[i] = g_scanner_cur_value (scanner).v_int;
	  }

	return TARGA_OK;

cleanup:
	if (t->data.id_string)
		g_free (t->data.id_string);
	if (t->data.color_map_data)
		g_free (t->data.color_map_data);
	if (t->data.image_data)
		g_free (t->data.image_data);

	return error;
}
