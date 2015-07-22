#include "../x.h"
#include "console.h"
#include "guile.h"
#include <libguile.h>

// One route to defining C functions into a module is
// scm_c_resolve_module to open and create if necessary a module
// SCM val = scm_c_make_gsubr(subr args and such)  to make it
// scm_c_module_define (module, c_name, val)
// but are they public?

// There is also scm_c_define_module where I could 

void
_burroscript_init (void *unused)
{
    console_init_guile_procedures();
}

void
init_lisp (void)
{
    /* Redirect output/error to console */
    // FIXME: do this

    // Define the Burro module
    scm_c_define_module ("burro", _burroscript_init, NULL);
    
    // Switch to a more useful module and use the Burro module
    SCM burro_user_module = scm_c_define_module ("guile-user", NULL, NULL);
    scm_set_current_module (burro_user_module);

    // Load scheme libraries
    char *path = getenv("BURRO_SCHEME_PATH");
    if (path == NULL)
        path = ".";
    if (path != NULL)
    {
        if (g_file_test (path, G_FILE_TEST_IS_DIR) == TRUE)
        {
            SCM old_path = scm_list_copy (scm_c_eval_string ("%load-path"));
            scm_set_car_x (scm_c_eval_string ("%load-path"),
                           scm_from_locale_string (path));
            scm_set_cdr_x (scm_c_eval_string ("%load-path"),
                           old_path);
        }
    }
    
    scm_c_use_module ("burro");
    scm_c_use_module ("engine");
    scm_c_use_module ("system repl repl");
  
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

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
