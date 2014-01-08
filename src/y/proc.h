#ifndef BURRO_PROC_H
#define BURRO_PROC_H
#include <glib.h>

void pm_init (void);
void pm_finalize (void);
void pm_queue_proc (GHook *proc);
void pm_iterate (double delta_t);

void proc_set_delta_time (double dt);
GHook *proc_wait_new (double dtime, gboolean (*child_func) (gpointer data), gpointer child_data);


#endif
