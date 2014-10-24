#include <config.h>

#include <glib.h>
#include <gio/gio.h>
#include "extern.h"

const guint
token_index[23] = {
    G_TOKEN_EOF,
  
  G_TOKEN_LEFT_PAREN,
  G_TOKEN_RIGHT_PAREN,
  G_TOKEN_LEFT_CURLY,
  G_TOKEN_RIGHT_CURLY,
  G_TOKEN_LEFT_BRACE,
  G_TOKEN_RIGHT_BRACE,
  G_TOKEN_EQUAL_SIGN,
  G_TOKEN_COMMA,
  
  G_TOKEN_NONE,
  
  G_TOKEN_ERROR,
  
  G_TOKEN_CHAR,
  G_TOKEN_BINARY,
  G_TOKEN_OCTAL,
  G_TOKEN_INT,
  G_TOKEN_HEX,
  G_TOKEN_FLOAT,
  G_TOKEN_STRING,
  
  G_TOKEN_SYMBOL,
  G_TOKEN_IDENTIFIER,
  G_TOKEN_IDENTIFIER_NULL,
  
  G_TOKEN_COMMENT_SINGLE,
  G_TOKEN_COMMENT_MULTI,
};

const char *
token_name[23] = 
{
    "EOF",
    "LEFT_PAREN",
    "RIGHT_PAREN",
    "LEFT_CURLY",
    "RIGHT_CURLY",
    "LEFT_BRACE",
    "RIGHT_BRACE",
    "EQUAL_SIGN",
    "COMMA",
    "NONE",
    "ERROR",
    "CHAR",
    "BINARY",
    "OCTAL",
    "INT",
    "HEX",
    "FLOAT",
    "STRING",
    "SYMBOL",
    "IDENTIFIER",
    "IDENTIFIER_NULL",
    "COMMENT_SINGLE",
    "COMMENT_MULTI"
};

static void
g_scanner_critical (const gchar *func, GTokenType expected, GTokenType received)
{
    GTokenType tt[2] = {expected, received};
    int ti[2];
    int i, j;
    for (j = 0; j < 2; j ++)
    {
        ti[j] = token_index[G_TOKEN_ERROR];
        for (i = 0; i < 23; i ++)
        {
            if (tt[j] == token_index[i])
            {
                ti[j] = i;
                break;
            }
        }
    }
    g_critical ("%s: expected %s but found %s", func, token_name[ti[0]], token_name[ti[1]]);
}

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
        g_critical ("g_io_channel_read_to_end returned ERROR: %s",
                (e && e->message) ? e->message : nm);
        break;
    case G_IO_STATUS_EOF:
        g_critical ("g_io_channel_read_to_end returned EOF: %s",
                (e && e->message) ? e->message : nm);
        break;
    case G_IO_STATUS_AGAIN:
    case G_IO_STATUS_NORMAL:
        // g_debug ("g_io_channel_read_to_end returned %d bytes", *length);
        break;
    }
}

GIOChannel *
xg_io_channel_unix_new               (int fd)
{
    GIOChannel *c;
    c = g_io_channel_unix_new (fd);
    if (c == NULL)
        g_critical ("g_io_channel_unix_new() returned NULL");
    return c;
}

gdouble
xg_scanner_get_next_token_float (GScanner *scanner)
{
    g_scanner_get_next_token (scanner);
    if (scanner->token != G_TOKEN_FLOAT)
    {
        g_scanner_critical (__func__, G_TOKEN_FLOAT, scanner->token);
        return 0;
    }
    return g_scanner_cur_value (scanner).v_float;
}

gulong
xg_scanner_get_next_token_hex (GScanner *scanner)
{
    g_scanner_get_next_token (scanner);
    if (scanner->token != G_TOKEN_HEX)
    {
        g_scanner_critical (__func__, G_TOKEN_HEX, scanner->token);
        return 0;
    }
    return g_scanner_cur_value (scanner).v_int;
}

gchar *
xg_scanner_get_next_token_identifier (GScanner *scanner)
{
    g_scanner_get_next_token (scanner);
    if (scanner->token != G_TOKEN_IDENTIFIER)
    {
        g_scanner_critical (__func__, G_TOKEN_IDENTIFIER, scanner->token);
        return NULL;
    }
    return g_scanner_cur_value (scanner).v_identifier;
}

gulong
xg_scanner_get_next_token_int (GScanner *scanner)
{
    g_scanner_get_next_token (scanner);
    if (scanner->token != G_TOKEN_INT)
    {
        g_scanner_critical (__func__, G_TOKEN_INT, scanner->token);
        return 0;
    }
    return g_scanner_cur_value (scanner).v_int;
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
        g_critical ("g_socket_client_connect_to_host() returned NULL");
    return c;
}

GSocketClient *     
xg_socket_client_new (void)
{
    GSocketClient *s = g_socket_client_new ();
    if (s == NULL)
        g_critical("g_socket_client_new() returned NULL");
    return s;
}

void
xg_socket_connect (GSocket *socket,
                   GSocketAddress *address)
{
    gboolean ret;
    GError *error = NULL;
    ret = g_socket_connect(socket, address, NULL, &error);
    if (ret == FALSE)
    {
        g_critical ("g_socket_connect returned FALSE: %s",
                    error->message);
    }
}

GSocket *
xg_socket_connection_get_socket (GSocketConnection *connection)
{
    GSocket *s;
    s = g_socket_connection_get_socket (connection);
    if (s == NULL)
        g_critical("g_socket_connection_get_socket() returned NULL");
    return s;
}

int
xg_socket_get_fd (GSocket *socket)
{
    int fd;
    fd = g_socket_get_fd(socket);
    if (fd == -1)
        g_critical("g_socket_get_fd() returned -1");
    return fd;
}

GSocket *
xg_socket_new (GSocketFamily family,
               GSocketType type,
               GSocketProtocol protocol)
{
    GSocket *s;
    GError *error;
    s = g_socket_new (family, type, protocol, &error);
    if (s == NULL)
        g_critical ("g_socket_new() returns NULL: %s", error->message);
    return s;
}

gssize
xg_socket_send (GSocket *socket,
                const gchar *buffer,
                gsize size)
{
    GError *error;
    gssize n;
    n = g_socket_send (socket, buffer, size, NULL, &error);
    if (n == -1)
    {
        g_critical("g_socket_send() returned -1: %s", error->message);
    }
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
        g_critical ("g_strsplit_set() returned NULL");
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

