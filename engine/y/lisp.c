#include "../x.h"
#include "console.h"
#include "guile.h"
#include <libguile.h>

void
init_lisp (void)
{
  /* Load Burro's Guile procedures into the burro environment. */
  SCM burro_module = scm_c_resolve_module ("burro");
  SCM old_module = scm_set_current_module (burro_module);
  init_guile_console_procedure ();
  init_guile_guile_procedures ();
  init_guile_obj_procedures ();

  /* Now switch back to the REPL's module */
  SCM use_module = scm_c_resolve_module ("guile-user");
  scm_set_current_module (use_module);
  scm_c_use_module ("burro");
}

#if 0
SCM_DEFINE (G_console, "console", 0, 0, 0, (void), "\
Suspend the editor and bring up the REPL for this buffer's module.")
{
  SCM var, func;

  /* Suspend the CRT screen handling.  */
  // interactive = false;
  // set_error_output_to_console ();

  console_reset ();
#define W console_write_latin1_string  
  W ("+-------------------------------------------------------+\n");
  W ("| This is the debug console for this buffer.            |\n");
  W ("| \"(quit)\" to quit back to the editor                   |\n");
  W ("| \"(burro-procedures)\" to see burro functions           |\n");
  W ("| \"(key-map)\" to see game's keymap                      |\n");
  W ("+-------------------------------------------------------+\n");
#undef W
  
  /* Switch to the current buffer's module.  */
  scm_c_use_module ("system repl repl");
  var = scm_c_lookup ("start-repl");
  func = scm_variable_ref (var);
  scm_call_0 (func);

  printf ("Press <ENTER> key to re-enter zile...\n");
  fflush (stdout);
  // set_error_output_to_minibuffer ();
  // scm_read_char (scm_current_input_port ());
  // interactive = true;
  return SCM_BOOL_T;
}

#endif

void
init_guile_lisp_procedures (void)
{
#include "lisp.x"
}
