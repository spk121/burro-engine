#include <stdbool.h>
#include <string.h>
#include "xgio.h"

int
xg_application_run (GApplication *application, int argc, char **argv)
{
    g_assert (application != NULL);
    g_assert (argc >= 1);
    int ret = g_application_run (application, argc, argv);

    if (ret != 0)
        g_warning ("g_application_run returns non-zero: %d\n", ret);

    return ret;
}

void
xg_seekable_seek (GSeekable *seekable, goffset offset, GSeekType type)
{
    gboolean ret;
    GError *err;
    ret = g_seekable_seek (seekable, offset, type, NULL, &err);
    if (ret == FALSE)
    {
        g_critical ("g_seekable_seek failed: %s", err->message);
        g_error_free (err);
    }
}

GInputStream *
xg_resources_open_stream (const char *path)
{
    GError *err = NULL;
    GInputStream *istream;
  
    g_return_val_if_fail (path != NULL && (strlen (path) > 0), NULL);
    istream = g_resources_open_stream (path, G_RESOURCE_LOOKUP_FLAGS_NONE, &err);
    if (istream == NULL)
    {
        g_critical ("g_resources_open_stream (%s) returned NULL: %s", path, err->message);
        g_error_free (err);
    }
    return istream;
}

void
xg_input_stream_close (GInputStream *stream)
{
    gboolean ret;
    GError *err = NULL;

    g_return_if_fail (stream != NULL);
    ret = g_input_stream_close (stream, NULL, &err);
    if (ret == FALSE)
    {
        g_critical ("g_input_stream_close failed: %s", err->message);
        g_error_free (err);
    }
}

static void
xg_input_stream_read_all (GInputStream *stream,
                          const void *id_string,
                          void *buffer,
                          size_t count,
                          size_t *bytes_read)
{
    bool ret;
    GError *err = NULL;

    g_return_if_fail (stream != NULL);
    g_return_if_fail (buffer != NULL);
    g_return_if_fail (count != 0);
    g_return_if_fail (bytes_read != NULL);
    
    ret = g_input_stream_read_all (stream, buffer, count, bytes_read, NULL, &err);
    if (ret == false)
    {
        if (id_string == NULL)
            g_critical ("g_input_steam_read_all() returned FALSE: %s", err->message);
        else
            g_critical ("g_input_stream_read_all(%s) returned FALSE: %s", (char *)id_string, err->message);
        g_error_free (err);
        *bytes_read = 0;
    }
}

size_t
xg_resources_get_filesize (const gchar *path)
{
    gboolean ret;
    GError *err = NULL;
    gsize sz = 0;
    guint32 flags = 0;

    g_return_val_if_fail (path != NULL && strlen (path) > 0, 0);

    ret = g_resources_get_info (path, G_RESOURCE_LOOKUP_FLAGS_NONE, &sz, &flags, &err);
    if (ret == FALSE)
    {
        g_critical ("g_resources_get_filesize(%s) returned FALSE: %s", path, err->message);
        g_error_free (err);
    }
    return sz;
}

char *
xg_resources_get_string (const char *path)
{
    size_t bytes_read = 0;
    
    g_return_val_if_fail (path != NULL, NULL);
    g_return_val_if_fail (strlen(path) > 0, NULL);
    
    GInputStream *stream = xg_resources_open_stream (path);
    size_t siz = xg_resources_get_filesize (path);
    char *str = (char *) g_malloc(siz + 1);

    xg_input_stream_read_all (stream, path, str, siz, &bytes_read);
    str[siz] = '\0';
    return str;
}

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
