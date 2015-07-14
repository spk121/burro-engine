#ifndef BURRO_XGLIB_H
#define BURRO_XGLIB_H

#include <glib.h>
#include <glib-object.h>

GMainLoop *         xg_default_main_loop_new            (void);
void                xg_file_get_contents                (const gchar *filename,
							   gchar **contents,
                                                           gsize *length);
char *              xg_find_data_file                   (const char *filename);
GHook *             xg_hook_alloc                       (GHookList *hl);
void                xg_hook_list_init                   (GHookList *hl);
guint               xg_idle_add                         (GSourceFunc function,
							   gpointer data);
void                xg_main_loop_quit                   (GMainLoop *loop);
void                xg_main_loop_run                    (GMainLoop *loop);
void                xg_main_loop_unref                  (GMainLoop *loop);
void                xg_object_unref                     (void *obj);

GRand *             xg_rand_new                         (void);
GRand *             xg_rand_new_with_seed               (guint32 seed);
gint32              xg_rand_int_range                   (GRand *rand, gint32 begin, gint32 end);
void                xg_set_application_name             (const gchar *application_name);
void                xg_setenv                           (const gchar *variable,
                                                         const gchar *value,
                                                         gboolean overwrite);
gulong              xg_signal_connect                   (gpointer instance,
                                                         const gchar *detailed_signal,
                                                         GCallback c_handler,
                                                         gpointer data);
gdouble             xg_timer_elapsed                    (GTimer *timer);
GTimer *            xg_timer_new                        (void);
void                xg_usleep                           (gulong microseconds);
#endif
