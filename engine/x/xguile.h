#ifndef BURRO_XGUILE_H
#define BURRO_XGUILE_H

#include <glib.h>
#include <libguile.h>

SCM                 xscm_c_eval_string                  (const gchar *string);
SCM                 xscm_c_primitive_load               (const gchar *filename);
void                xscm_init_guile                     (void);

#endif
