/*  xglib.h

    Copyright (C) 2018   Michael L. Gran
    This file is part of Burro Engine

    Burro Engine is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Burro Engine is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Burro Engine.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef BURRO_XGLIB_H
#define BURRO_XGLIB_H

#include <glib.h>
#include <glib-object.h>
#include <stdint.h>

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
char *              xg_strndup                          (const char *str, size_t n);
gdouble             xg_timer_elapsed                    (GTimer *timer);
GTimer *            xg_timer_new                        (void);
void                xg_usleep                           (gulong microseconds);
uint32_t *          xg_utf8_to_ucs4                     (const char *u8);

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
