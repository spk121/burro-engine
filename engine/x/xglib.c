#include <string.h>
#include <glib.h>
#include "xglib.h"

GMainLoop *
xg_default_main_loop_new (void)
{
  GMainLoop *ml;
  ml = g_main_loop_new (NULL, FALSE);
  if (ml == NULL)
    g_critical ("g_main_loop_new returned FALSE");
  return ml;
}

void
xg_file_get_contents (const gchar *filename, gchar **contents, gsize *length)
{
  gboolean ret;
  GError *err;
  g_return_if_fail (filename != NULL && (strlen (filename) > 0));
  g_return_if_fail (contents != NULL);
  g_return_if_fail (length != NULL);
  ret = g_file_get_contents (filename, contents, length, &err);
  if (ret != TRUE)
    {
      g_critical ("g_file_get_contents(%s) failed: %s", filename, err->message);
      g_error_free (err);
    }
}

/* Finds the data directory for a given filename.  Returns a string to
 * be freed by the caller.  */
char *
xg_find_data_file (const char *filename)
{
    const char *user_dir = g_get_user_data_dir ();
    const char * const *sys_dirs = g_get_system_data_dirs();

    if (user_dir != NULL)
    {
        char *path = g_build_path (G_DIR_SEPARATOR_S, user_dir, "burro", filename, NULL);
        if (g_file_test (path, G_FILE_TEST_IS_REGULAR) == TRUE)
            return path;
        else
            g_free (path);
    }

    size_t i = 0;
    while (sys_dirs[i] != NULL)
    {
        char *path = g_build_path (G_DIR_SEPARATOR_S, sys_dirs[i], "burro", filename, NULL);
        if (g_file_test (path, G_FILE_TEST_IS_REGULAR) == TRUE)
            return path;

        g_free (path);
        i++;
    }

    return NULL;
}

GHook *
xg_hook_alloc (GHookList *hl)
{
  GHook *h;

  g_return_val_if_fail (hl != NULL, 0);
  h = g_hook_alloc (hl);
  if (h == NULL)
    g_critical ("g_hook_alloc returned NULL");
  return h;
}


void
xg_hook_list_init (GHookList *hl)
{
  g_hook_list_init (hl, sizeof (GHook));
}

guint
xg_idle_add (GSourceFunc function, gpointer data)
{
  guint id;
  g_return_val_if_fail (function != NULL, 0);
  id = g_idle_add (function, data);
  if (id == 0)
    g_critical ("g_idle_add returned zero");
  return id;
}

void               
xg_main_loop_quit (GMainLoop *loop)
{
  g_return_if_fail (loop != NULL);
  g_main_loop_quit (loop);
}

void                
xg_main_loop_run (GMainLoop *loop)
{
  g_return_if_fail (loop != NULL);
  g_main_loop_run (loop);
}

void                
xg_main_loop_unref (GMainLoop *loop)
{
  g_return_if_fail (loop != NULL);
  g_main_loop_unref(loop);
}

void
xg_object_unref (void *obj)
{
    g_return_if_fail (obj != NULL);
    g_object_unref (obj);
}

GRand *
xg_rand_new (void)
{
  GRand *r = g_rand_new ();
  if (r == NULL)
    g_critical ("g_rand_new returned NULL");
  return r;
}

GRand *
xg_rand_new_with_seed (guint32 seed)
{
  GRand *r = g_rand_new_with_seed (seed);
  if (r == NULL)
    g_critical ("g_rand_new_with_seed returned NULL");
  return r;
}

gint32
xg_rand_int_range (GRand *rand, gint32 begin, gint32 end)
{
  g_return_val_if_fail (rand != NULL, 0);
  return g_rand_int_range (rand, begin, end);
}

void
xg_set_application_name (const gchar *application_name)
{
  g_set_application_name (application_name);
}

void
xg_setenv (const gchar *variable, const gchar *value, gboolean overwrite)
{
  gboolean r;

  g_return_if_fail (variable != NULL && (strlen (variable) > 0));
  g_return_if_fail (value != NULL && (strlen (value) > 0));

  r = g_setenv (variable, value, overwrite);
  if (r == FALSE)
    g_critical ("g_setenv returned FALSE");
}

gulong              
xg_signal_connect (gpointer instance, const gchar *detailed_signal, GCallback c_handler, gpointer data)
{
  gulong id;

  g_return_val_if_fail (instance != NULL, 0);
  g_return_val_if_fail (detailed_signal != NULL && (strlen (detailed_signal) > 0), 0);
  g_return_val_if_fail (c_handler != NULL, 0);

  id = g_signal_connect_data (instance, detailed_signal, c_handler, data, NULL, (GConnectFlags) 0);
  return id;
}

char *
xg_strndup (const char *str, size_t n)
{
    g_return_val_if_fail (str != NULL, NULL);
    if (n == 0)
        g_critical ("n == 0 in xg_strndup");
    return g_strndup(str, n);
}

gdouble
xg_timer_elapsed (GTimer *timer)
{
  gdouble t;
  g_return_val_if_fail (timer != NULL,  0.0);
  
  t = g_timer_elapsed (timer, NULL);
  return t;
}

GTimer *            
xg_timer_new (void)
{
  GTimer *t;
  t = g_timer_new ();
  if (t == NULL)
    g_critical ("g_timer_new returned NULL");
  return t;
}

void               
xg_usleep (gulong microseconds)
{
  g_usleep (microseconds);
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
