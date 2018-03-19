#pragma once
#include <glib.h>
#include <gio/gio.h>

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
