#ifndef BURRO_EXTERN_H
#define BURRO_EXTERN_H
#include <glib.h>
#include <gio/gio.h>

#ifdef ENABLE_DEBUG
# define xdebug(...) g_debug(__VA_ARGS__);
#else
# define xdebug(...)
#endif

#ifdef ENABLE_STRICT
# ifdef ENABLE_LOGGING
#  define xerror(...) g_error(__VA_ARGS__)
# else
#  define xerror(...) exit(1);
# endif
#else
# ifdef ENABLE_LOGGING
#  define xerror(...) g_critical(__VA_ARGS__)
# else
#  define xerror(...)
# endif
#endif


guint               xg_io_add_watch                      (GIOChannel *channel,
							  GIOCondition condition,
							  GIOFunc func,
							  gpointer user_data);
void                xg_io_channel_read_to_end            (GIOChannel *channel,
							  gchar **str_return,
							  gsize *length);
GIOChannel *        xg_io_channel_unix_new               (int fd);
GSocketConnection * xg_socket_client_connect_to_host     (GSocketClient *client,
                                                          const gchar *host_and_port,
                                                          guint16 default_port);
GSocketClient *     xg_socket_client_new                 (void);
GSocket *           xg_socket_connection_get_socket      (GSocketConnection *connection);
int                 xg_socket_get_fd                     (GSocket *socket);
gboolean            xg_str_has_prefix                    (const gchar *str,
							  const gchar *prefix);
gchar **            xg_strsplit_set                      (const gchar *string,
							  const gchar *delimiters,
							  gint max_tokens);
#endif
/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
