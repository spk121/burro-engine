#ifndef IO_H_INCLUDED
#define IO_H_INCLUDED

#include "../x.h"

/** Register an ISO9660 image as a resource collection.
 *  First, look for it in BURRO_DATA_DIR.
 *  Second, look for it in the current working directory
 */
bool io_open_iso (const char *name);

void io_close_iso ();

/** Fill a memory location with data from a resource.
 *   If the buffer is not large enough, fill as much as possible.
 *   - First, check for the an unpacked data in the ISO, if one
 *     has been opened.
 *   - Second, check for the resource in BURRO_DATA_DIR
 *
 *   The data in the resource will be interpreted based on the
 *   filename extension.
 *
 *   PNG files will be unpacked as 2D ARGB32 data
 *   PCM files will be unpacked as raw I32LE data
 *   MAP files will be unpacked as raw U32 data
 *
 *   @param name
 *       The filename of a resource
 *   @param buf
 *       The location to which the resource will be copied
 *   @param maxlen
 *       The maximum size (in bytes) that can be placed in BUF
 *   @param len
 *       The actual number of bytes placed in BUF
 *   @return TRUE if the resource was found
 */

/**  U32 2D data is stored with an 8-byte header of 2 4-byte integers: width and height
 *   followed by U32 data
 */
bool io_fill_u32_2D_data (const char *name,
                          uint32_t **buf, int max_width, int max_height,
                          int *width, int *height);

/** I16 data is stored as 1 32-bit integer which is the length followed by the
 *  I16 data
 */
bool io_fill_i16_data (const char *name,
                           int16_t *buf, int max_length,
                           int *length);

/** TXT data is store as 1 32-bit header which is a 4-byte integer length
 *   followed by 7-bit ASCII encoded data
 */
bool io_fill_ascii_data(const char *name, char *buf, int max_length, int *length);


#if 0
#define DECLARE_SREAD_TYPE(TYPE)					\
	TYPE sread_ ## TYPE (GInputStream *istream);

#define DECLARE_SREAD_TYPE_LE(TYPE)				\
	TYPE sread_ ## TYPE ## _LE(GInputStream *istream);

#define DECLARE_SREAD_ARRAY_TYPE(TYPE)									\
	void sread_ ## TYPE ## _array (TYPE *data, gsize count, GInputStream *istream);

gsize sread (void *data, gsize size, gsize count, GInputStream *istream);
gsize sread_rle_array (void *data, gsize size, gsize count, GInputStream *istream);

DECLARE_SREAD_ARRAY_TYPE(gchar)
DECLARE_SREAD_ARRAY_TYPE(guint8)

DECLARE_SREAD_TYPE(guint8)

DECLARE_SREAD_TYPE_LE(guint16)
DECLARE_SREAD_TYPE_LE(guint32)

#define DECLARE_SWRITE_ARRAY_TYPE(TYPE)									\
	void swrite_ ## TYPE ## _array (TYPE *data, gsize count, GOutputStream *istream);

#define DECLARE_SWRITE_TYPE(TYPE)							\
	void swrite_ ## TYPE (TYPE x, GOutputStream *ostream);

#define DECLARE_SWRITE_TYPE_LE(TYPE)								\
	void swrite_ ## TYPE ## _LE (TYPE x, GOutputStream *ostream);

DECLARE_SWRITE_ARRAY_TYPE(gchar)
DECLARE_SWRITE_ARRAY_TYPE(guint8)

DECLARE_SWRITE_TYPE(guint8)

DECLARE_SWRITE_TYPE_LE(guint16)
DECLARE_SWRITE_TYPE_LE(guint32)
#endif

#endif // IO_H_INCLUDED
