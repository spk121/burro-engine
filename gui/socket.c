#include <string.h>
#include "socket.h"
#include "extern.h"
#include "commands.h"

#define SOCKET_PORT (50002)

GSocket *m_socket;

static gboolean
read_socket_cb(gpointer data)
{
    GSocket *sock = data;
    gchar str[256];
    gssize n;
    n = g_socket_receive_with_blocking(sock, str, 256, FALSE, NULL, NULL);
    if (n > 0)
    {
        g_debug("Received message: \"%s\"", str);
        do_commands_from_string (str, n);
    }
    return TRUE;
}


void
initialize_socket()
{
    GSocketAddress *socket_address;
    GInetAddress *inet_address;
    const char *hello = "HELLO\r\n";
    
    m_socket = xg_socket_new (G_SOCKET_FAMILY_IPV4,
                              G_SOCKET_TYPE_STREAM,
                              G_SOCKET_PROTOCOL_TCP);
    inet_address = g_inet_address_new_from_string("127.0.0.1");
    socket_address = g_inet_socket_address_new (inet_address, SOCKET_PORT);
    g_object_unref (inet_address);
    xg_socket_connect (m_socket, socket_address);

    /* Declare oneself to the game server */
    xg_socket_send (m_socket, hello, strlen (hello));

    g_timeout_add(10, read_socket_cb, m_socket);
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
