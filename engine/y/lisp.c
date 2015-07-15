
void
init_lisp (void)
{
  /* Load Burro's Guile procedures into the burro environment. */
  xscm_c_resolve_module ("burro");
  init_guile_guile_procedures ();
  init_guile_obj_procedures ();
}

SCM_DEFINE (G_console, "console", 0, 0, 0, (void), "\
Suspend the editor and bring up the REPL for this buffer's module.")
{
  SCM var, func;

  /* Suspend the CRT screen handling.  */
  interactive = false;
  set_error_output_to_console ();

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
  set_error_output_to_minibuffer ();
  scm_read_char (scm_current_input_port ());
  interactive = true;
  return SCM_BOOL_T;
}

void
init_guile_lisp_procedures (void)
{
#include "lisp.x"
  scm_c_export ("console",
		NULL);
}
