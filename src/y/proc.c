#include "../x/xglib.h"
#include "proc.h"

/* The Process Manager (PM) is a list of hook functions and associated
   data.  Each function in the list is called on its associated data
   during each on_idle.  If a function returns FALSE, it is removed
   from the PM.

*/

static GHookList *pm;
double delta_time = 0.0;

void proc_set_delta_time (double dt)
{
  delta_time = dt;
}

/* Initializes the process manager */
void
pm_init ()
{
  pm = g_new0 (GHookList, 1);
  xg_hook_list_init (pm);
}

void
pm_finalize ()
{
  g_free (pm);
}

void
pm_queue_proc (GHook *proc)
{
  g_hook_append (pm, proc);
}

void
pm_iterate (double delta_t)
{
  proc_set_delta_time (delta_t);
  //xg_hook_list_invoke_check (pm, FALSE);
}

/* Wait process -- waits for time to pass */
struct proc_wait_data_tag
{
  double start_time;
  double stop_time;
  gboolean (*child_func) (gpointer data);
  gpointer child_data;
};

typedef struct proc_wait_data_tag proc_wait_data_t;

/* Wait for time to pass, then add the child func to the PM */
static gboolean proc_wait_func (gpointer data)
{
  proc_wait_data_t *wdata = (proc_wait_data_t *) data;

  g_assert (data != NULL);

  wdata->start_time += delta_time;
  if (wdata->start_time > wdata->stop_time)
    {
      if (wdata->child_func == NULL)
	g_error ("wait process has no child func");
      if (wdata->child_data == NULL)
	g_error ("wait process has no child data");

      GHook *h = g_hook_alloc (pm);
      g_assert (h != NULL);

      h->data = wdata->child_data;
      h->func = wdata->child_func;
      g_hook_append (pm, h);
      wdata->child_func = NULL;

      return FALSE;
    }
  else
    return TRUE;
}

GHook *
proc_wait_new (double dtime, gboolean (*child_func) (gpointer data), gpointer child_data)
{
  g_assert (dtime > 0.0);
  g_assert (child_func != NULL);
  g_assert (child_data != NULL);

  GHook *hook = xg_hook_alloc (pm);
  proc_wait_data_t *wait_data = g_new0 (proc_wait_data_t, 1);
  wait_data->start_time = 0.0;
  wait_data->stop_time = dtime;
  wait_data->child_func = child_func;
  wait_data->child_data = child_data;
  hook->data = wait_data;
  hook->func = proc_wait_func;
  return hook;
}
  
  


  
