#include <string.h>
#include "socket.h"
#include "extern.h"
#include "commands.h"

#define SOCKET_PORT (50002)

GSocket *m_socket;

static gboolean
io_timer_cb(gpointer data)
{
    GIOChannel *ch = data;
    gchar *str;
    gsize length;
    //xg_io_channel_read_to_end(ch, &str, &length);
    g_io_channel_read_line(ch, &str, &length, NULL, NULL);
    if (length > 0)
    {
        g_debug("Received message: \"%s\"", str);
        do_commands_from_string (str, length);
    }
    g_free (str);
    g_io_channel_write_chars(ch, "WHAT TIME IS IT?", strlen("WHAT TIME IS IT?"),
                             NULL, NULL);

    return TRUE;
}

static gboolean
read_socket_cb(gpointer data)
{
    GSocket *sock = data;
    gchar str[256];
    gsize length;
    gssize n;
    n = g_socket_receive(sock, str, 256, NULL, NULL);
    if (n > 0)
    {
        g_debug("Received message: \"%s\"", str);
        do_commands_from_string (str, n);
        xg_socket_send (m_socket, "WHAT TIME IS IT?", strlen("WHAT TIME IS IT?"));

    }
    //g_io_channel_write_chars(ch, "WHAT TIME IS IT?", strlen("WHAT TIME IS IT?"),
    //                       NULL, NULL);

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
    
    m_socket = xg_socket_new (G_SOCKET_FAMILY_IPV4,
                              G_SOCKET_TYPE_STREAM,
                              G_SOCKET_PROTOCOL_TCP);
    // address = g_network_address_new ("127.0.0.1", SOCKET_PORT);
    // socket_address = g_socket_address_enumerator_next (g_socket_connectable_enumerate (address), NULL, NULL);
    socket_address = g_inet_socket_address_new (g_inet_address_new_from_string("127.0.0.1"), SOCKET_PORT);
    xg_socket_connect (m_socket, socket_address);
    /* Declare oneself to the game server */
    xg_socket_send (m_socket, hello, strlen (hello));

    /* g_timeout_add(10, io_timer_cb,  */
    /*               g_io_channel_unix_new (g_socket_get_fd (m_socket))); */
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
