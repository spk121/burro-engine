// targa.c -- Functions related to the parsing of Targa files

#include <glib.h>
#include <glib/gprintf.h>
#include <string.h>
#include "tga.h"
#include "io.h"


#define MEM0(dest) \
	memset((void *)(dest),0,sizeof(dest))

targa_error_t targa_parse_stream (GInputStream *istream, targa_image_t *t)
{
    guint filesize;
    targa_error_t error;
    size_t id_size = 0;
    size_t color_map_size = 0;
    size_t image_unpacked_size = 0;

    if (!g_seekable_seek (G_SEEKABLE(istream), 0, G_SEEK_END, NULL, NULL))
    {
        error = TARGA_READ_ERROR;
        g_printerr ("read error");
        goto cleanup;
    }
    filesize = g_seekable_tell (G_SEEKABLE(istream)) + 1;
    g_seekable_seek (G_SEEKABLE(istream), 0, G_SEEK_SET, NULL, NULL);

    MEM0 (&(t->header));
    MEM0 (&(t->data));

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

guint targa_get_color_map_size (const targa_image_t *t)
{
    return t->header.color_map_length;
}

guint targa_get_color_map_first_index (const targa_image_t *t)
{
    return t->header.color_map_first_entry_index;
}
