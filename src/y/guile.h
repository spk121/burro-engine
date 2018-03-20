#ifndef BURRO_GUILE_H
#define BURRO_GUILE_H

#include "../x/xguile.h"
#include <stdbool.h>
#include "bg.h"

char *               guile_any_to_c_string                    (SCM x);
SCM                  guile_c_eval_string_safe                 (const char *string);
SCM                  guile_error_handler                      (void *data,
							       SCM key,
							       SCM exception);
bool                 guile_get_procedure_arity                (SCM proc,
							       int *required,
							       int *optional);
SCM                  guile_lookup_procedure                    (const char *name);
bool                 guile_symbol_is_name_of_defined_function (SCM sym);
unsigned             guile_to_ranged_uint_or_error            (const char *function_name,
							       int position,
							       unsigned max,
							       SCM n);
SCM                  guile_use_burro_module                   (void *unused);
SCM                  guile_variable_ref_safe                  (SCM var);
#endif
void
guile_show_unassigned_bg_error (const char *function_name, bg_index_t bg);
void
guile_vram_error (const char *function_name, vram_bank_t bank);
void
guile_init_guile_procedures (void);

