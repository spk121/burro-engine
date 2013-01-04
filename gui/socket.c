#include <string.h>
#include "socket.h"
#include "extern.h"
#include "commands.h"

#define SOCKET_PORT (50002)

GSocket *m_socket;

static gboolean io_watch_cb(GIOChannel *channel, GIOCondition cond, gpointer data)
{
    gchar *str;
    gsize length;

    xg_io_channel_read_to_end(channel, &str, &length);
    if (length > 0)
    {
        do_commands_from_string (str, length);
        g_free (str);
    }
    else
        return FALSE;

    return TRUE;
}

static gboolean
io_timer_cb(gpointer data)
{
    GIOChannel *ch = data;
    gchar *str;
    gsize length;
    xg_io_channel_read_to_end(ch, &str, &length);
    if (length > 0)
    {
        do_commands_from_string (str, length);
    }
    g_free (str);

    return TRUE;
}

void
initialize_socket()
{
    GSocketConnectable *address;
    GSocketAddress *socket_address;
    int fd;
    GIOChannel *channel;
    const char *hello = "HELLO\r\n";
    
    m_socket = g_socket_new (G_SOCKET_FAMILY_IPV4,
                             G_SOCKET_TYPE_STREAM,
                             G_SOCKET_PROTOCOL_TCP,
                             NULL);
    address = g_network_address_new ("localhost", SOCKET_PORT);
    socket_address = g_socket_address_enumerator_next (g_socket_connectable_enumerate (address), NULL, NULL);
    g_socket_connect (m_socket, socket_address, NULL, NULL);

    /* Declare oneself to the game server */
    xg_socket_send (m_socket, hello, strlen (hello));

    g_timeout_add(10, io_timer_cb, 
                  g_io_channel_unix_new (g_socket_get_fd (m_socket)));
}

void
finalize_socket ()
{
    g_object_unref(m_socket);
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
