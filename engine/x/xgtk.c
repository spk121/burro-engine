#include <string.h>
#include <gtk/gtk.h>
#include "xgtk.h"

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
xgtk_window_set_position (GtkWindow *window, GtkWindowPosition position)
{
  g_return_if_fail (window != NULL);
  g_return_if_fail (strcmp(G_OBJECT_TYPE_NAME(window), "GtkWindow") == 0);
  gtk_window_set_position (window, position);
}

void
xgtk_widget_set_size_request (GtkWidget *widget, gint width, gint height)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (width >= -1);
  g_return_if_fail (height >= -1);
  gtk_widget_set_size_request (widget, width, height);
}

void
xgtk_window_set_title (GtkWindow *window, const gchar *title)
{
  g_return_if_fail (window != NULL);
  g_return_if_fail (title != NULL && (strlen (title) > 0));
  gtk_window_set_title (window, title);
}
