#include "socket.h"
#include "extern.h"
#include "commands.h"

GSocketClient *m_socket_client;

static gboolean io_watch_cb(GIOChannel *channel, GIOCondition cond, gpointer data)
{
    gchar *str;
    gsize length;

    xg_io_channel_read_to_end(channel, &str, &length);
    do_commands_from_string (str, length);
    g_free (str);

    return TRUE;
}

void
initialize_socket()
{
    GSocketConnection *socket_connection;
    GSocket *socket;
    int fd;
    GIOChannel *channel;

    m_socket_client = xg_socket_client_new();
    socket_connection = xg_socket_client_connect_to_host(m_socket_client, "localhost", 7772);
    socket = xg_socket_connection_get_socket(socket_connection);
    fd = xg_socket_get_fd(socket);
    channel = xg_io_channel_unix_new(fd);
    xg_io_add_watch(channel, G_IO_IN, io_watch_cb, NULL);
}

void
finalize_socket ()
{
    g_object_unref(m_socket_client);
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
