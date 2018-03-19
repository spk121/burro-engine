#include <glib.h>
#include <gio/gio.h>

#include "io.h"

/* common read functions */

#define guint16_TO_LE(x) GUINT16_TO_LE(x)
#define guint32_TO_LE(x) GUINT32_TO_LE(x)
#define guint16_FROM_LE(x) GUINT16_FROM_LE(x)
#define guint32_FROM_LE(x) GUINT32_FROM_LE(x)

#define DEFINE_SREAD_TYPE(TYPE)					\
	TYPE sread_ ## TYPE (GInputStream *istream) \
	{											\
		TYPE x;									\
		sread (&x, sizeof (TYPE), 1, istream);	\
		return x;								\
	}

#define DEFINE_SREAD_TYPE_LE(TYPE)								\
	TYPE sread_ ## TYPE ## _LE(GInputStream *istream)			\
	{															\
		TYPE x;													\
		sread (&x, sizeof (TYPE), 1, istream);					\
		return TYPE ## _FROM_LE (x);							\
	}

#define DEFINE_SREAD_ARRAY_TYPE(TYPE)									\
	void sread_ ## TYPE ## _array (TYPE *data, gsize count, GInputStream *istream) \
	{																	\
		sread (data, sizeof (TYPE), count, istream);					\
	}

#define DEFINE_SWRITE_TYPE(TYPE)							\
	void swrite_ ## TYPE (TYPE x, GOutputStream *ostream)	\
	{														\
		swrite (&x, sizeof (TYPE), 1, ostream);				\
	}

#define DEFINE_SWRITE_TYPE_LE(TYPE)									\
	void swrite_ ## TYPE ## _LE (TYPE x, GOutputStream *ostream)	\
	{																\
		TYPE x_LE = TYPE ## _TO_LE(x);								\
		swrite (&x_LE, sizeof (TYPE), 1, ostream);					\
	}

#define DEFINE_SWRITE_ARRAY_TYPE(TYPE)									\
	void swrite_ ## TYPE ## _array (TYPE *data, gsize count, GOutputStream *istream) \
	{																	\
		swrite (data, sizeof (TYPE), count, istream);					\
	}


gsize sread (void *data, gsize size, gsize count, GInputStream *istream)
{
	gboolean ret;
	gsize bytes_read;
	GError *err = NULL;

	if (size == 0 || count == 0)
		return 0;

	ret = g_input_stream_read_all (istream, data, size * count, &bytes_read, NULL, &err);
	if (ret == FALSE)
	{
		g_printerr ("Unable to read file: %s\n", err->message);
		g_error_free (err);
	}
	return bytes_read / size;
}

gsize sread_rle_array (void *data, gsize size, gsize count, GInputStream *istream)
{
	guint16 x = 0;
	guint i = 0;
	guint8 packet_header;
	guint8 packet_type;
	guint8 packet_count;
	guint8 packet_index;

	while (x < count * size)
	{
		/* This is a header byte */
		packet_header = sread_guint8 (istream);
		packet_type = packet_header & 0x80;
		packet_count = (packet_header & 0x7f) + 1;
		packet_index = 0;
		if (packet_type)
		{
			// This packet represents repeated values
			guint8 repeated_value[4];
			for (i = 0; i < size; i ++)
				repeated_value[i] = sread_guint8 (istream);
			while (packet_index < packet_count)
			{
				for (i = 0; i < size; i ++)
					((guint8 *)data)[x++] = repeated_value[i];
				packet_index ++;
			}
		}
		else
		{
			// This packet contains just raw values
			while (packet_index < packet_count)
			{
				for (i = 0; i < size; i ++)
					((guint8 *)data)[x++] = sread_guint8 (istream);
				packet_index ++;
			}
		}
	}
	return x;
}

DEFINE_SREAD_ARRAY_TYPE(gchar)
DEFINE_SREAD_ARRAY_TYPE(guint8)

DEFINE_SREAD_TYPE(guint8)

DEFINE_SREAD_TYPE_LE(guint16)
DEFINE_SREAD_TYPE_LE(guint32)


gsize swrite (void *data, gsize size, gsize count, GOutputStream *ostream)
{
	gboolean ret;
	gsize bytes_written;
	GError *err = NULL;

	if (size == 0 || count == 0)
		return 0;
	ret = g_output_stream_write_all (ostream, data, size * count, &bytes_written, NULL, &err);
	if (ret == FALSE)
	{
		g_printerr ("Unable to write file: %s\n", err->message);
		g_error_free (err);
	}
	return bytes_written / size;
}


DEFINE_SWRITE_ARRAY_TYPE(gchar)
DEFINE_SWRITE_ARRAY_TYPE(guint8)

DEFINE_SWRITE_TYPE(guint8)

DEFINE_SWRITE_TYPE_LE(guint16)
DEFINE_SWRITE_TYPE_LE(guint32)
