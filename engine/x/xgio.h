#ifndef BURRO_XGIO_H
#define BURRO_XGIO_H
#include <glib.h>
#include <gio/gio.h>
void                xg_seekable_seek                    (GSeekable *seekable,
                                                         goffset offset,
                                                         GSeekType type);
GInputStream *      xg_resources_open_stream            (const gchar *path);
void                xg_input_stream_close               (GInputStream *stream);
gsize               xg_resources_get_filesize           (const gchar *path);
#endif
