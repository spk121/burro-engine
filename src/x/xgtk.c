/*  xgtk.c

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
#include <string.h>
#include <gtk/gtk.h>
#include "xgtk.h"

GList *
xgtk_application_get_windows (GtkApplication *application)
{
    g_assert (application != NULL);
    return gtk_application_get_windows (application);
}

void
xgtk_container_add (GtkContainer *container, GtkWidget *widget)
{
    g_return_if_fail (container != NULL);
    g_return_if_fail (widget != NULL);
    gtk_container_add (container, widget);
}

void
xgtk_container_set_border_width (GtkContainer *container, guint border_width)
{
    g_return_if_fail (container != NULL);
    gtk_container_set_border_width (container, border_width);
}

GtkWidget *
xgtk_drawing_area_new (void)
{
    GtkWidget *w;
    w = gtk_drawing_area_new ();
    if (w == NULL)
        g_critical ("gtk_drawing_area_new returned NULL");
    return w;
}

GtkWidget *
xgtk_fixed_new (void)
{
    GtkWidget *w;
    w = gtk_fixed_new ();
    if (w == NULL)
        g_critical ("gtk_fixed_new returned NULL");
    return w;
}

void
xgtk_fixed_put (GtkFixed *fixed, GtkWidget *widget, gint x, gint y)
{
    g_return_if_fail (fixed != NULL);
    g_return_if_fail (widget != NULL);
    g_return_if_fail (x >= 0);
    g_return_if_fail (y >= 0);
    gtk_fixed_put (fixed, widget, x, y);
}

void
xgtk_init_check (int *argc, char ***argv)
{
    gboolean ret;
    ret = gtk_init_check (argc, argv);
    if (ret == FALSE)
        g_critical ("GTK initialization failed");
}

GdkWindow *
xgtk_widget_get_window (GtkWidget *widget)
{
    GdkWindow *w;
    g_return_val_if_fail (widget != NULL, NULL);
    w = gtk_widget_get_window (widget);
    if (w == NULL)
        g_critical ("gtk_widget_get_window returned NULL");
    return w;
}

void
xgtk_widget_realize (GtkWidget *widget)
{
    g_return_if_fail (widget != NULL);
    gtk_widget_realize (widget);
}

void
xgtk_widget_show_all (GtkWidget *widget)
{
    g_return_if_fail (widget != NULL);
    gtk_widget_show_all (widget);
}

void
xgtk_widget_set_size_request (GtkWidget *widget, gint width, gint height)
{
    g_return_if_fail (widget != NULL);
    g_return_if_fail (width >= -1);
    g_return_if_fail (height >= -1);
    gtk_widget_set_size_request (widget, width, height);
}

GtkWidget *
xgtk_window_new (GtkWindowType type)
{
    GtkWidget *w;
    w = gtk_window_new (type);
    if (w == NULL)
        g_critical ("gtk_widget_new returned NULL");
    return w;
}

void
xgtk_window_set_application (GtkWindow *window, GtkApplication *application)
{
    g_assert (window != NULL);
    g_assert (application  != NULL);
    gtk_window_set_application (window, application);
}

void
xgtk_window_set_position (GtkWindow *window, GtkWindowPosition position)
{
    g_return_if_fail (window != NULL);
    g_return_if_fail (strcmp(G_OBJECT_TYPE_NAME(window), "GtkWindow") == 0);
    gtk_window_set_position (window, position);
}

void
xgtk_window_set_default_icon_name (const gchar *name)
{
    g_return_if_fail (name != NULL);
    gtk_window_set_default_icon_name (name);
}

void
xgtk_window_set_title (GtkWindow *window, const gchar *title)
{
    g_return_if_fail (window != NULL);
    g_return_if_fail (title != NULL && (strlen (title) > 0));
    gtk_window_set_title (window, title);
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
