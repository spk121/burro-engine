#include <config.h>

#include <glib.h>
#include <gio/gio.h>
#include "extern.h"



guint
xg_io_add_watch                      (GIOChannel *channel,
                                      GIOCondition condition,
                                      GIOFunc func,
                                      gpointer user_data)
{
    return g_io_add_watch(channel, condition, func, user_data);
}

void
xg_io_channel_read_to_end            (GIOChannel *channel,
                                      gchar **str_return,
                                      gsize *length)
{
    GIOStatus s;
    GError *e = NULL;
    const char *nm = "(no message)";

    s = g_io_channel_read_to_end (channel, str_return, length, &e);
    switch (s) {
    case G_IO_STATUS_ERROR:
        xerror ("g_io_channel_read_to_end returned ERROR: %s",
                (e && e->message) ? e->message : nm);
        break;
    case G_IO_STATUS_EOF:
        xerror ("g_io_channel_read_to_end returned EOF: %s",
                (e && e->message) ? e->message : nm);
        break;
    case G_IO_STATUS_AGAIN:
    case G_IO_STATUS_NORMAL:
        // xdebug ("g_io_channel_read_to_end returned %d bytes", *length);
        break;
    }
}

GIOChannel *
xg_io_channel_unix_new               (int fd)
{
    GIOChannel *c;
    c = g_io_channel_unix_new (fd);
    if (c == NULL)
        xerror ("g_io_channel_unix_new() returned NULL");
    return c;
}

GSocketConnection *
xg_socket_client_connect_to_host     (GSocketClient *client,
                                      const gchar *host_and_port,
                                      guint16 default_port)
{
    GSocketConnection *c;
    c = g_socket_client_connect_to_host (client,
                                         host_and_port,
                                         default_port,
                                         NULL,
                                         NULL);
    if (c == NULL)
        xerror ("g_socket_client_connect_to_host() returned NULL");
    return c;
}

GSocketClient *     
xg_socket_client_new (void)
{
    GSocketClient *s = g_socket_client_new ();
    if (s == NULL)
        xerror("g_socket_client_new() returned NULL");
    return s;
}

GSocket *
xg_socket_connection_get_socket (GSocketConnection *connection)
{
    GSocket *s;
    s = g_socket_connection_get_socket (connection);
    if (s == NULL)
        xerror("g_socket_connection_get_socket() returned NULL");
    return s;
}

int
xg_socket_get_fd (GSocket *socket)
{
    int fd;
    fd = g_socket_get_fd(socket);
    if (fd == -1)
        xerror("g_socket_get_fd() returned -1");
    return fd;
}

gssize
xg_socket_send (GSocket *socket,
                const gchar *buffer,
                gsize size)
{
    gssize n;
    n = g_socket_send (socket, buffer, size, NULL, NULL);
    if (n == -1)
        xerror("g_socket_send() returned -1");
    return n;
}

gboolean
xg_str_has_prefix (const gchar *str, const gchar *prefix)
{
    return g_str_has_prefix (str, prefix);
}

gchar **
xg_strsplit_set (const gchar *string,
                 const gchar *delimiters,
                 gint         max_tokens)
{
    gchar **strv;
    strv = g_strsplit_set (string, delimiters, max_tokens);
    if (strv == NULL)
        xerror ("g_strsplit_set() returned NULL");
    return strv;
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

