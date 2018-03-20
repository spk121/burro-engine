/*  xgtk.h

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
    along with Burro Engin.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef BURRO_XGTK_H
#define BURRO_XGTK_H

#include <gtk/gtk.h>
#include <gdk/gdk.h>
GList *             xgtk_application_get_windows        (GtkApplication *application);
void                xgtk_container_add                  (GtkContainer *container,
                                                         GtkWidget *widget);
void                xgtk_container_set_border_width     (GtkContainer *container,
                                                         guint border_width);
GtkWidget *         xgtk_drawing_area_new               (void);
GtkWidget *         xgtk_fixed_new                      (void);
void                xgtk_fixed_put                      (GtkFixed *fixed,
                                                         GtkWidget *widget,
                                                         gint x,
                                                         gint y);
void                xgtk_init_check                     (int *argc,
                                                         char ***argv);
GdkWindow *         xgtk_widget_get_window              (GtkWidget *widget);
void                xgtk_widget_realize                 (GtkWidget *widget);
void                xgtk_widget_set_size_request        (GtkWidget *widget,
                                                         gint width,
                                                         gint height);
void                xgtk_widget_show_all                (GtkWidget *widget);
GtkWidget *         xgtk_window_new                     (GtkWindowType type);
void                xgtk_window_set_application         (GtkWindow *window,
							 GtkApplication *application);
void                xgtk_window_set_default_icon_name (const gchar *name);
void                xgtk_window_set_position            (GtkWindow *window,
                                                         GtkWindowPosition position);
void                xgtk_window_set_title               (GtkWindow *window,
                                                         const gchar *title);
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
