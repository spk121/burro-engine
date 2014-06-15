// tga.c -- Functions related to the parsing of Tga files

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include "libtga.h"
#include "libtga-private.h"
#include "color.h"

static uint32_t
get_u32(const uint8_t *pos)
{
  uint32_t a, b, c, d;
  a = pos[0];
  b = pos[1];
  c = pos[2];
  d = pos[3];
  return a | b << 8 | c << 16 | d << 24;
}

static uint32_t
get_u16(const uint8_t *pos)
{
  uint16_t a, b;
  a = pos[0];
  b = pos[1];
  return a | b << 8;
}


// DEST must be pre-allocated to be able to hold UNIT_COUNT
// units of UNIT_SIZE.  Returns size of uncompressed area.
static size_t
sread_rle_array (tga_ctx_t *ctx, uint8_t *dest, size_t unit_size,
		 size_t unit_count, const uint8_t *src, size_t src_len,
		 size_t *src_len_used)
{
  size_t dest_len = unit_size * unit_count;
  size_t i_dest = 0;
  size_t i;
  uint8_t packet_header;
  uint8_t packet_type;
  uint8_t packet_count;
  uint8_t packet_index;
  size_t i_src = 0;
  const size_t max_unit_size = 4;

  if (unit_size > max_unit_size) {
    err (ctx, "Invalid unit size for RLE array: %u", (unsigned int) unit_size);
    return 0;
  }

  while (i_dest < dest_len) {
    if (i_src >= src_len) {
      err (ctx, "premature termination (RLE array)");
      return 0;
    }
    packet_header = src[i_src++];
    packet_type = packet_header & 0x80;
    packet_count = (packet_header & 0x7f) + 1;
    packet_index = 0;
    if (packet_type) {
      // This packet represents repeated values
      uint8_t repeated_value[max_unit_size];
      for (i = 0; i < unit_size; i ++) {
        if (i_src >= src_len) {
          err (ctx, "premature termination (RLE array)");
          return 0;
        }
        repeated_value[i] = src[i_src++];
      }
      while (packet_index < packet_count) {
        for (i = 0; i < unit_size; i ++) {
          if (i_dest >= dest_len) {
            err (ctx, "destination buffer too small (RLE array)");
            return 0;
          }
          dest[i_dest++] = repeated_value[i];
        }
        packet_index ++;
      }
    }
    else {
      // This packet contains just raw values
      while (packet_index < packet_count) {
        for (i = 0; i < unit_size; i ++) {
          if (i_src >= src_len) {
            err (ctx, "premature termination (RLE array)");
            return 0;
          }
          if (i_dest >= dest_len) {
            err (ctx, "destination buffer too small (RLE array)");
            return 0;
          }
          dest[i_dest++] = src[i_src++];
        }
        packet_index ++;
      }
    }
  }
  *src_len_used = i_src;
  return i_dest;
}

/* This is a paranoid parser for Tga files. It should fail on most
 * errors. It allocates a new image. */
TGA_LOCAL tga_error_t
unpack_memory (const uint8_t *mem, size_t len, tga_image_t *t)
{
  int i;
  const uint8_t *raw_footer;
  tga_error_t error;
  size_t id_size = 0;
  size_t color_map_size = 0;
  size_t image_unpacked_size = 0;

  t->version = 1;
  t->extended_info = 0;

  /* Since it is impossible to validate the length of the file without
   * parsing most of the file, we'll end up checking its size many
   * different times as we learn more about the file.
   */

  /* First, we see if it at least contains a header. */
  if (len < TGA_PACKED_HEADER_LEN) {
    error = TGA_HEADER_TOO_SHORT;
    err (t->ctx, "the data ends prematurely (header)");
    goto cleanup;
  }

  // Then we check to see if this is a Tga file v2.0
  if (len < TGA_PACKED_HEADER_LEN + TGA_PACKED_FOOTER_LEN) {
    t->version = 1;
  }
  else {
    // A TGA reader should begin by determining whether the file is New
    // TGA by checking the 26-byte footer.  Scan bytes 8 to 23 of the
    // footer for the signature.
    raw_footer = mem + len - TGA_PACKED_FOOTER_LEN;
    if (memcmp(raw_footer + TGA_PACKED_FOOTER_STRING_START,
               "TRUEVISION-XFILE",
               TGA_PACKED_FOOTER_STRING_LEN) == 0)
      t->version = 2;
    else
      t->version = 1;
  }
  // If it is a 2.0 file, we can properly parse the footer
  if (t->version == 2) {
    // Field 28 - Extension Area Offset
    t->footer.extension_offset = get_u32(raw_footer + 0);

    // Field 29 - Developer Directory Offset
    t->footer.developer_area_offset = get_u32(raw_footer + 4);;

    // Field 30 - Signature
    memcpy((void *) t->footer.signature, "TRUEVISION-XFILE", 16);

    // Field 31 - Reserved Character
    t->footer.signature[16] = '.';

    // Field 32 - Binary Zero String Terminator
    t->footer.signature[17] = '\0';
  }

  if (t->footer.extension_offset)
    t->extended_info = 1;

  /* Now we know if it has a header, a footer, an extension, and a
   * developer area.  We've done the footer.  Now let's do the rest. */

  // Field 1 - ID Length
  t->header.id_string_length = mem[0];

  // Field 2 - Color Map Type
  t->header.color_map_type = mem[1];
  if (!TGA_COLOR_MAP_TYPE_VALID(t)) {
    error = TGA_COLOR_MAP_TYPE_OUT_OF_RANGE;
    err (t->ctx, "unknown color map type: %u",
         (unsigned int) t->header.color_map_type);
    goto cleanup;
  }

  // Field 3 - Image Type
  t->header.image_type = (tga_image_type_t) mem[2];
  if (!TGA_IMAGE_TYPE_VALID(t)) {
    error = TGA_IMAGE_TYPE_OUT_OF_RANGE;
    err (t->ctx, "invalid image type: %u",
         (unsigned int) t->header.image_type);
    goto cleanup;
  }
  if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_NO_COLOR_MAP
      && TGA_IMAGE_TYPE_COLOR_MAPPED(t)) {
    error = TGA_MISSING_COLOR_MAP;
    err (t->ctx, "image type %u requires a color map",
         (unsigned int) t->header.image_type);
    goto cleanup;
  }
  if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP
      && !TGA_IMAGE_TYPE_COLOR_MAPPED(t)) {
    error = TGA_UNNECESSARY_COLOR_MAP;
    err (t->ctx, "image type %u should not have a color map",
         (unsigned int) t->header.image_type);
    goto cleanup;
  }

  // Field 4 - Color Map Specification

  // Field 4.1 - First Entry Index
  t->header.color_map_first_entry_index = get_u16(mem + 3);
  if (t->header.color_map_first_entry_index != 0
      && t->header.color_map_type == TGA_COLOR_MAP_TYPE_NO_COLOR_MAP) {
    error = TGA_COLOR_MAP_FIRST_ENTRY_INDEX_INVALID;
    err (t->ctx, "color map first entry index %u must be zero when there is no color map",
         (unsigned int) t->header.color_map_first_entry_index);
    goto cleanup;
  }

  // Field 4.2 - Color Map Length
  t->header.color_map_length = get_u16(mem + 5);
  if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_NO_COLOR_MAP
      && t->header.color_map_length > 0) {
    error = TGA_COLOR_MAP_LENGTH_INVALID;
    err (t->ctx, "color map length %u must be zero when there is no color map",
         (unsigned int) t->header.color_map_length);
    goto cleanup;
  }

  // Field 4.3 - Color Map Entry Size
  if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP
      && t->header.color_map_length == 0) {
    error = TGA_COLOR_MAP_LENGTH_INVALID;
    err (t->ctx, "color map length %u must be greater than zero when there is a color map",
         (unsigned int) t->header.color_map_length);
    goto cleanup;
  }
  if (t->header.color_map_first_entry_index > t->header.color_map_length) {
    error = TGA_COLOR_MAP_FIRST_ENTRY_INDEX_OUT_OF_RANGE;
    err (t->ctx, "color map first entry index %u must be less than the color map length %u",
         (unsigned int) t->header.color_map_first_entry_index,
         (unsigned int) t->header.color_map_length);
    goto cleanup;
  }
  t->header.color_map_entry_size = mem[7];
  if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_NO_COLOR_MAP
      && t->header.color_map_entry_size != 0) {
    error = TGA_COLOR_MAP_ENTRY_SIZE_INVALID;
    err (t->ctx, "color map width %u must be zero when there is no color map",
         (unsigned int) t->header.color_map_entry_size);
    goto cleanup;
  }
  if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP
      && !TGA_COLOR_MAP_ENTRY_SIZE_VALID(t)) {
    error = TGA_COLOR_MAP_ENTRY_SIZE_OUT_OF_RANGE;
    err (t->ctx, "color map width %u is not one of the commonly allowed values",
         (unsigned int) t->header.color_map_entry_size);
    goto cleanup;
  }

  // 15bpp is a perfectly valid color map width, but, I don't want to
  // support it.
  if (t->header.color_map_entry_size == 15)
    t->header.color_map_entry_size = 16;

  // Field 5 - Image Specification
  // Field 5.1 - X Origin of Image
  t->header.x_origin = get_u16(mem + 8);

  // Field 5.2 - Y Origin of Image
  t->header.y_origin = get_u16(mem + 10);

  // Field 5.3 - Image Width
  t->header.image_width = get_u16(mem + 12);

  // Field 5.4 - Image Height
  t->header.image_height = get_u16(mem + 14);

  // Field 5.5 - Pixel Depth
  t->header.pixel_depth = mem[16];
  if ((TGA_IMAGE_TYPE_COLOR_MAPPED(t)
       && (t->header.pixel_depth == TGA_PIXEL_DEPTH_8_BITS
           || t->header.pixel_depth == TGA_PIXEL_DEPTH_16_BITS))
      || (TGA_IMAGE_TYPE_TRUE_COLOR(t)
          && (t->header.pixel_depth == TGA_PIXEL_DEPTH_15_BITS
              || t->header.pixel_depth == TGA_PIXEL_DEPTH_16_BITS
              || t->header.pixel_depth == TGA_PIXEL_DEPTH_24_BITS
              || t->header.pixel_depth == TGA_PIXEL_DEPTH_32_BITS))
      || (TGA_IMAGE_TYPE_BLACK_AND_WHITE(t)
          && (t->header.pixel_depth == TGA_PIXEL_DEPTH_8_BITS
              || t->header.pixel_depth == TGA_PIXEL_DEPTH_16_BITS))) {
    // Pixel depth is good
  }
  else {
    error = TGA_PIXEL_DEPTH_OUT_OF_RANGE;
    err (t->ctx, "pixel depth %u is not one of the commonly allowed values",
         (unsigned int) t->header.pixel_depth);
    goto cleanup;
  }

  // 15bpp is a perfectly valid pixel depth, but, I don't want to
  // support it.
  if (t->header.pixel_depth == TGA_PIXEL_DEPTH_15_BITS)
    t->header.pixel_depth = TGA_PIXEL_DEPTH_16_BITS;

  // Field 5.6 - Image Descriptor
  t->header.image_descriptor = mem[17];
  if (t->header.image_descriptor & 0xC0) {
    error = TGA_IMAGE_DESCRIPTOR_OUT_OF_RANGE;
    err (t->ctx, "image descriptor %u is out of range",
         (unsigned int) t->header.image_descriptor);
    goto cleanup;
  }

  // Field 6 - Image ID (Variable)
  id_size = t->header.id_string_length;
  if ((t->version == 1 && len < TGA_PACKED_HEADER_LEN + id_size)
      || (t->version == 2 && len < (TGA_PACKED_HEADER_LEN
                                    + TGA_PACKED_FOOTER_LEN + id_size))) {
    error = TGA_ID_STRING_TOO_SHORT;
    err (t->ctx, "the file ends prematurely (ID string)");
    goto cleanup;
  }

  uint8_t *pos = (uint8_t *) mem + 18;
  if (id_size == 0)
    t->data.id_string = NULL;
  else {
    t->data.id_string = calloc (id_size + 1, sizeof(char));
    memcpy(t->data.id_string, pos, id_size);
    t->data.id_string[id_size] = '\0';
    pos += id_size;
  }

  // Field 7 - Color Map Data (variable)
  if (t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP)
    color_map_size = ((size_t) t->header.color_map_length
                      * (size_t) BIT_TO_BYTE(t->header.color_map_entry_size));
  else
    color_map_size = 0;

  if (((t->version == 1)
       && (len < TGA_PACKED_HEADER_LEN + id_size + color_map_size))
      ||
      ((t->version == 2)
       && (len < (TGA_PACKED_HEADER_LEN + id_size + color_map_size
                  + TGA_PACKED_FOOTER_LEN)))) {
    error = TGA_COLOR_MAP_TOO_SHORT;
    err (t->ctx, "the file ends prematurely (color map)");
    goto cleanup;
  }

  if (color_map_size > 0) {
    t->data.color_map_data = calloc (color_map_size, sizeof(uint8_t));
    memcpy(t->data.color_map_data, pos, color_map_size);
    pos += color_map_size;
  }
  else
    t->data.color_map_data = NULL;

  // Field 8 - Image Data
  if (t->header.image_type == TGA_IMAGE_TYPE_NO_IMAGE_DATA) {
    image_unpacked_size = 0;
    t->data.image_data = NULL;
  }
  else {
    image_unpacked_size = (TGA_IMAGE_SIZE(t)
                           * BIT_TO_BYTE (t->header.pixel_depth));
    t->data.image_data = calloc(image_unpacked_size, sizeof(uint8_t));

    // How to read the unpack the data depends on the format
    if (TGA_IMAGE_TYPE_UNCOMPRESSED(t)) {
      // For uncompressed data, we can sanity check the file size
      // FIXME: size check wrong for version 2
      if (len < (TGA_PACKED_HEADER_LEN + id_size + color_map_size
                 + image_unpacked_size)) {
        error = TGA_IMAGE_DATA_TOO_SHORT;
        err (t->ctx, "the file ends prematurely (image data)");
      }
      memcpy(t->data.image_data, pos, image_unpacked_size);
      pos += image_unpacked_size;
    }
    else if (TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED(t)) {
      int j;
      size_t compressed_area_len;
      uint8_t *start_pos = pos;
      compressed_area_len = (len -
                             TGA_PACKED_HEADER_LEN
                             - id_size
                             - color_map_size);
      if (t->version == 2)
        compressed_area_len -= TGA_PACKED_FOOTER_LEN;

      // For RLE data, we can't sanity check the file size first.
      for (j = 0; j < t->header.image_height; j ++) {
        size_t count, compressed_count, row_size;
        uint8_t *row_pos;
        row_size = t->header.image_width * BIT_TO_BYTE (t->header.pixel_depth);
        row_pos = t->data.image_data + j * row_size;
        count = sread_rle_array (t->ctx, row_pos,
                                 BIT_TO_BYTE (t->header.pixel_depth),
                                 t->header.image_width,
                                 pos,
                                 compressed_area_len,
                                 &compressed_count);
        if (count == 0) {
          error = TGA_IMAGE_DATA_TOO_SHORT;
          err (t->ctx, "the file ends prematurely (RLE image data)");
          goto cleanup;
        }
        dbg(t->ctx, "j %u row_size %zu row_pos %ld, %ld count %zu ccount %zu\n",
	       j, row_size, row_pos - t->data.image_data,
	       pos - start_pos,
	       count, compressed_count);
	image_unpacked_size += count;
	pos += compressed_count;
	compressed_area_len -= compressed_count;
      }
      /* Since we've uncompressed the data... */
      if (t->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_COLOR_MAPPED)
	t->header.image_type = TGA_IMAGE_TYPE_UNCOMPRESSED_COLOR_MAPPED;
      else if (t->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_TRUE_COLOR)
	t->header.image_type = TGA_IMAGE_TYPE_UNCOMPRESSED_TRUE_COLOR;
      else if (t->header.image_type == TGA_IMAGE_TYPE_RUN_LENGTH_ENCODED_BLACK_AND_WHITE)
	t->header.image_type = TGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE;
    }
  }

  // Field 9 - Developer Data (variable)
  // This is vendor specific, so we ignore it
  if (t->footer.extension_offset) {
    if (len < (t->footer.extension_offset
	       + TGA_EXTENSION_AREA_LEN
	       + TGA_PACKED_FOOTER_LEN)) {
      error = TGA_EXTENSION_AREA_TOO_SHORT;
      err (t->ctx, "the file ends prematurely (extension area)");
      goto cleanup;
    }
    pos = (uint8_t *) mem + t->footer.extension_offset;

    // Field 10 - Extension Size (2 Bytes)
    t->extension.extension_size = get_u16(pos);
    pos += 2;
    if (t->extension.extension_size != TGA_EXTENSION_SIZE) {
      error = TGA_EXTENSION_SIZE_OUT_OF_RANGE;
      err (t->ctx, "extension size %u is out of range (must equal %u)",
	   (unsigned int) t->extension.extension_size,
	   (unsigned int) TGA_EXTENSION_SIZE);
      goto cleanup;
    }

    // Field 11 - Author Name (41 Bytes)
    memcpy(t->extension.author_name, pos, TGA_AUTHOR_NAME_LEN);
    pos += TGA_AUTHOR_NAME_LEN;
    if (t->extension.author_name[TGA_AUTHOR_NAME_LEN-1] != '\0') {
      error = TGA_AUTHOR_NAME_NOT_NULL_TERMINATED;
      err (t->ctx, "the author name is not null terminated");
      goto cleanup;
    }

    // Field 12 - Author Comments (324 Bytes)
    for (i = 0; i < TGA_AUTHOR_COMMENT_LINES_NUM; i ++) {
      memcpy(t->extension.author_comment[i], pos,
	     TGA_AUTHOR_COMMENT_LINE_LEN);
      pos += TGA_AUTHOR_COMMENT_LINE_LEN;
      if (t->extension.author_comment[i][TGA_AUTHOR_COMMENT_LINE_LEN-1] != '\0') {
	error = TGA_AUTHOR_COMMENT_NOT_NULL_TERMINATED;
	err (t->ctx, "author comment line %u is not null terminated",
	     (unsigned int) i);
	goto cleanup;
      }
    }

    // Field 13 - Date/Time Stamp (12 Bytes)
    t->extension.month = get_u16(pos);
    pos += 2;
    if (t->extension.month < 1 || t->extension.month > 12) {
      error = TGA_MONTH_OUT_OF_RANGE;
      goto cleanup;
    }
    t->extension.day = get_u16(pos);
    pos += 2;
    if (t->extension.day < 1 || t->extension.day > 31) {
      error = TGA_DAY_OUT_OF_RANGE;
      goto cleanup;
    }
    t->extension.year = get_u16(pos);
    pos += 2;
    t->extension.hour = get_u16(pos);
    pos += 2;
    if (t->extension.hour > 23) {
      error = TGA_HOUR_OUT_OF_RANGE;
      goto cleanup;
    }
    t->extension.minute = get_u16(pos);
    pos += 2;
    if (t->extension.minute > 59) {
      error = TGA_MINUTE_OUT_OF_RANGE;
      goto cleanup;
    }
    t->extension.second = get_u16(pos);
    pos += 2;
    if (t->extension.second > 59) {
      error = TGA_SECOND_OUT_OF_RANGE;
      goto cleanup;
    }

    // Field 14 - Job Name/ID (41 Bytes)
    memcpy(t->extension.job_id, pos, TGA_JOB_ID_LEN);
    pos += TGA_JOB_ID_LEN;
    if (t->extension.job_id[TGA_JOB_ID_LEN-1] != '\0') {
      error = TGA_JOB_ID_NOT_NULL_TERMINATED;
      goto cleanup;
    }

    // Field 15 - Job Time (6 Bytes)
    t->extension.job_hour = get_u16(pos);
    pos += 2;
    t->extension.job_minute = get_u16(pos);
    pos += 2;
    if (t->extension.job_minute > 59) {
      error = TGA_JOB_MINUTE_OUT_OF_RANGE;
      goto cleanup;
    }
    t->extension.job_second = get_u16(pos);
    pos += 2;
    if (t->extension.job_second > 59) {
      error = TGA_JOB_SECOND_OUT_OF_RANGE;
      goto cleanup;
    }

    // Field 16 - Software ID (41 Bytes)
    memcpy(t->extension.software_id, pos, TGA_SOFTWARE_ID_LEN);
    pos += TGA_SOFTWARE_ID_LEN;
    if (t->extension.software_id[TGA_SOFTWARE_ID_LEN-1] != '\0') {
      error = TGA_SOFTWARE_ID_NOT_NULL_TERMINATED;
      goto cleanup;
    }

    // Field 17 - Software Version (3 Bytes)
    t->extension.version_number = get_u16(pos);
    pos += 2;
    t->extension.version_letter = (char) *pos;
    pos ++;
    // FIXME should test that version letter is a letter or space here

    // Field 18 - Key Color (4 Bytes)
    t->extension.key_color_argb = get_u32(pos);
    pos += 4;

    // Field 19 - Pixel Aspect Ratio (4 Bytes)
    t->extension.pixel_aspect_ratio_numerator = get_u16(pos);
    pos += 2;
    t->extension.pixel_aspect_ratio_denominator = get_u16(pos);
    pos += 2;

    // Field 20 - Gamma Value (4 Bytes)
    t->extension.gamma_correction_factor_numerator = get_u16(pos);
    pos += 2;
    t->extension.gamma_correction_factor_denominator = get_u16(pos);
    pos += 2;
    if (t->extension.gamma_correction_factor_denominator != 0) {
      double gamma_correction = ((double) t->extension.gamma_correction_factor_numerator
				 / (double) t->extension.gamma_correction_factor_denominator);
      if (gamma_correction > 10.0) {
	error = TGA_GAMMA_CORRECTION_OUT_OF_RANGE;
	goto cleanup;
      }
    }

    // Field 21 - Color Correction Offset (4 Bytes)
    t->extension.color_correction_offset = get_u32(pos);
    pos += 4;

    // Field 22 - Postage Stamp Offset (4 Bytes)
    t->extension.stamp_offset = get_u32(pos);
    pos += 4;

    // Field 23 - Scan Line Offset (4 Bytes)
    t->extension.scan_line_offset = get_u32(pos);
    pos += 4;

    // Field 24 - Attributes Type (1 Byte)
    t->extension.alpha_attribute = *pos;
    pos ++;
    if (!TGA_ALPHA_ATTRIBUTES_VALID(t)) {
      error = TGA_ALPHA_ATTRIBUTES_OUT_OF_RANGE;
      err (t->ctx, "alpha attributes %u out of range",
	   (unsigned int) t->extension.alpha_attribute);
      goto cleanup;
    }
    if ((t->extension.alpha_attribute == TGA_ALPHA_ATTRIBUTES_NO_ALPHA)
	&& ((t->header.image_descriptor & 0x0F) != 0)) {
      error = TGA_ALPHA_ATTRIBUTES_CONFLICT;
      err (t->ctx, "header has no alpha attribute set but has %u alpha channel bit(s) set",
	   (unsigned int)(t->header.image_descriptor & 0x0F));
    }

    // Field 25 - Scan Line Table
    // We don't read the scan line table.  It exists as an optimization
    // to let a program read in only part of an image.  We always unpack
    // the whole image.

    // Field 26 - Postage Stamp Image
    // We don't read in the postage stamp image, which is a thumbnail
    // image, because, we only care about the full image.

    // Field 27 - Color Correction Table
    // FIXME: read the color correction table
  }

  return TGA_OK;

cleanup:
  if (t->data.id_string)
    free (t->data.id_string);
  if (t->data.color_map_data)
    free (t->data.color_map_data);
  if (t->data.image_data)
    free (t->data.image_data);

  return error;
}

void
tga_image_free (const tga_image_t *t)
{
  if (t->data.id_string)
    free (t->data.id_string);
  if (t->data.color_map_data)
    free (t->data.color_map_data);
  if (t->data.image_data)
    free (t->data.image_data);
}

bool tga_has_image (const tga_image_t *t)
{
	if (t->header.image_type != TGA_IMAGE_TYPE_NO_IMAGE_DATA)
		return 1;
	return 0;
}

bool
tga_has_palette (const tga_image_t *t)
{
  int test1, test2, test3;

  /* Some applications were known to define color maps for true color
   * images so they could use the color map area for other things.
   * This is why the image_type is the more important indicator of
   * whether something has a color map. */
  test1 = TGA_IMAGE_TYPE_COLOR_MAPPED(t);
  test2 = t->header.color_map_type == TGA_COLOR_MAP_TYPE_COLOR_MAP;
  test3 = t->header.color_map_length > 0;
  if (test1 && test2 && test3)
    return 1;
  return 0;
}

void tga_get_image_dimensions (const tga_image_t *t,
				 unsigned int *width, unsigned int *height)
{
  if (tga_has_image (t)) {
    *width = t->header.image_width;
    *height = t->header.image_height;
  }
  else {
    *width = 0;
    *height = 0;
  }
}

void tga_get_image_orientation (const tga_image_t *t, tga_hflip_t *hflip, tga_vflip_t *vflip)
{
  if (tga_has_image (t)) {
    *hflip = t->header.image_descriptor & 0b00010000 ? 1 : 0;
    *vflip = t->header.image_descriptor & 0b00100000 ? 1 : 0;
  }
  else {
    *hflip = 0;
    *vflip = 0;
  }
}

TGA_LOCAL color_format_t
tga_get_image_color_format (const tga_image_t *t)
{
  color_format_t c;
  uint8_t bpp = t->header.pixel_depth;

  if (TGA_IMAGE_TYPE_BLACK_AND_WHITE(t)) {
    if (bpp == 8)
      c = COLOR_g8;
    else if (bpp == 16)
      c = COLOR_g16;
    else
      c = COLOR_x;
  }
  else if (TGA_IMAGE_TYPE_COLOR_MAPPED(t)) {
    if (bpp == 8)
      c = COLOR_i8;
    else if (bpp == 16)
      c = COLOR_i16;
    else
      c = COLOR_x;
  }
  else if (TGA_IMAGE_TYPE_TRUE_COLOR(t)) {
    unsigned int attribute = t->header.image_descriptor & 0x0f;

    switch (bpp) {
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


color_format_t
tga_get_palette_color_format (const tga_image_t *t)
{
  color_format_t c;

  if (TGA_IMAGE_TYPE_COLOR_MAPPED(t)) {
    uint8_t bpp = t->header.color_map_entry_size;
		unsigned int attribute = t->header.image_descriptor & 0x0f;

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


unsigned int
tga_get_color_map_size (const tga_image_t *t)
{
	return t->header.color_map_length;
}

unsigned int
tga_get_color_map_first_index (const tga_image_t *t)
{
	return t->header.color_map_first_entry_index;
}

#if 0
tga_error_t tga_write (GOutputStream *ostream, tga_image_t *t)
{
	t->header.y_origin = 0;
	t->header.image_descriptor &= 0x0f;

	swrite_uint8_t (t->header.id_string_length, ostream);
	swrite_uint8_t (t->header.color_map_type, ostream);
	swrite_uint8_t (t->header.image_type, ostream);
	swrite_guint16_LE (t->header.color_map_first_entry_index, ostream);
	swrite_guint16_LE (t->header.color_map_length, ostream);
	swrite_uint8_t (t->header.color_map_entry_size, ostream);
	swrite_guint16_LE (t->header.x_origin, ostream);
	swrite_guint16_LE (t->header.y_origin, ostream);
	swrite_guint16_LE (t->header.image_width, ostream);
	swrite_guint16_LE (t->header.image_height, ostream);
	swrite_uint8_t (t->header.pixel_depth, ostream);
	swrite_uint8_t (t->header.image_descriptor, ostream);
	if (t->header.id_string_length)
		swrite_gchar_array (t->data.id_string, t->header.id_string_length, ostream);
	if (t->header.color_map_length)
		swrite_uint8_t_array (t->data.color_map_data,
							 (int) t->header.color_map_length * BIT_TO_BYTE(t->header.color_map_entry_size),
							 ostream);
	if (t->header.image_width > 0 && t->header.image_height > 0)
		swrite_uint8_t_array (t->data.image_data,
							 TGA_IMAGE_SIZE(t) * BIT_TO_BYTE(t->header.pixel_depth),
							 ostream);
	return TGA_OK;
}

tga_error_t map_parse_stream (GInputStream *istream, tga_image_t *t)
{
	gchar *text;
	guint filesize, bytes_read;
	tga_error_t error;
	size_t image_unpacked_size = 0;
	GScanner *scanner;

	t->version = 1;
	t->extended_info = 0;

	/* The 1st line contains x dimension and y dimension */
bool            g_input_stream_read_all             (GInputStream *stream,
                                                         void *buffer,
                                                         gsize count,
                                                         gsize *bytes_read,
                                                         GCancellable *cancellable,
                                                         GError **error);

	if (!g_seekable_seek (G_SEEKABLE(istream), 0, G_SEEK_END, NULL, NULL))
	{
		error = TGA_READ_ERROR;
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
	if (filesize < TGA_PACKED_HEADER_LEN)
	{
		error = TGA_HEADER_TOO_SHORT;
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
	t->header.color_map_type = TGA_COLOR_MAP_TYPE_NO_COLOR_MAP;

	// Field 3 - Image Type
	t->header.image_type = TGA_IMAGE_TYPE_UNCOMPRESSED_BLACK_AND_WHITE;

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
	t->data.color_map_data = (uint8_t *) 0;

	// Field 8 - Image Data
	image_unpacked_size = TGA_IMAGE_SIZE(t) * BIT_TO_BYTE (t->header.pixel_depth);
	t->data.image_data = g_new0 (uint8_t, image_unpacked_size);
	for (int i = 0; i < TGA_IMAGE_SIZE (t); i ++)
	  {
	    g_scanner_get_next_token (scanner);
	    ((guint16 *) (t->data.image_data))[i] = g_scanner_cur_value (scanner).v_int;
	  }

	return TGA_OK;

cleanup:
	if (t->data.id_string)
		g_free (t->data.id_string);
	if (t->data.color_map_data)
		g_free (t->data.color_map_data);
	if (t->data.image_data)
		g_free (t->data.image_data);

	return error;
}
#endif
