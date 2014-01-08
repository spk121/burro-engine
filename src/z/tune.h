#ifndef BURRO_TUNE_H
#define BURRO_TUNE_H

/* This module loads chiptunes from the data store and sets up procs
   that can play them. */

proc_tune_t *
proc_tune_load_from_resource (guint resource_id);

#endif
