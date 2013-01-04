#include <string.h>
#include "socket.h"
#include "extern.h"
#include "commands.h"

#define SOCKET_PORT (50002)

GSocketClient *m_socket_client;

static gboolean io_watch_cb(GIOChannel *channel, GIOCondition cond, gpointer data)
{
    gchar *str;
    gsize length;

    xg_io_channel_read_to_end(channel, &str, &length);
    if (length > 0)
    {
        do_commands_from_string (str, length);
        g_free (str);

        return TRUE;
    }
    return FALSE;
}

void
initialize_socket()
{
    GSocketConnection *socket_connection;
    GSocket *socket;
    int fd;
    GIOChannel *channel;
    const char *hello = "HELLO\r\n";

    m_socket_client = xg_socket_client_new();
    socket_connection = xg_socket_client_connect_to_host(m_socket_client, "localhost", SOCKET_PORT);
    socket = xg_socket_connection_get_socket(socket_connection);
    fd = xg_socket_get_fd(socket);

    channel = xg_io_channel_unix_new(fd);

    g_io_channel_set_flags(channel, 
                           g_io_channel_get_flags(channel) & (~G_IO_FLAG_NONBLOCK),
                           NULL);

    /* Declare oneself to the game server */
    xg_socket_send (socket, hello, strlen (hello));

    /* Setup callback */
    xg_io_add_watch(channel, G_IO_IN, io_watch_cb, NULL);

    g_io_channel_set_close_on_unref (channel, TRUE);
    g_io_channel_unref (channel);
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
