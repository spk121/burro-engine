#include <string.h>
#include "xgio.h"

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


gsize
xg_resources_get_filesize (const gchar *path)
{
  gboolean ret;
  GError *err = NULL;
  gsize sz = 0;
  guint32 flags = 0;

  g_return_val_if_fail (path != NULL && strlen (path) > 0, 0);

  ret = g_resources_get_info (path, 0, &sz, &flags, &err);
  if (ret == FALSE)
    {
      g_critical ("g_resources_get_filesize(%s) returned FALSE: %s", path, err->message);
      g_error_free (err);
    }
  return sz;
}

