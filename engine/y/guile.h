#ifndef BURRO_GUILE_H
#define BURRO_GUILE_H

#include "../x/xguile.h"
#include <stdbool.h>

bool                 guile_get_procedure_arity                (SCM proc,
							       int *required,
							       int *optional);
bool                 guile_symbol_is_name_of_defined_function (SCM sym);
unsigned             guile_to_ranged_uint_or_error            (const char *function_name,
							       int position,
							       unsigned max,
							       SCM n);

#endif
