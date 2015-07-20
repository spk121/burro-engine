#ifndef BURRO_XGUILE_H
#define BURRO_XGUILE_H

#include <stdbool.h>
#include <libguile.h>

SCM                 xscm_c_eval_string                  (const char *string);
SCM                 xscm_c_primitive_load               (const char *filename);
SCM                 xscm_from_latin1_symbol             (const char *name);
void                xscm_init_guile                     (void);
SCM                 xscm_lookup                         (SCM name);
bool                xscm_is_variable                    (SCM x);
bool                xscm_is_symbol                      (SCM x);
bool                xscm_is_procedure                   (SCM x);
#endif
