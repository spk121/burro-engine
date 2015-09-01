#include "../x.h"
#include <glib.h>
#include "audio_model.h"
#include "backdrop.h"
#include "bg.h"
#include "console.h"
#include "eng.h"
#include "guile.h"
#include "lisp.h"
#include "loop.h"
#include "repl.h"
#include "sheet.h"
#include <libguile.h>

static char *lisp_main_script = NULL;

// One route to defining C functions into a module is
// scm_c_resolve_module to open and create if necessary a module
// SCM val = scm_c_make_gsubr(subr args and such)  to make it
// scm_c_module_define (module, c_name, val)
// but are they public?

// There is also scm_c_define_module where I could 

void
_burroscript_init (void *unused)
{
    am_init_guile_procedures ();
    backdrop_init_guile_procedures ();
    bg_init_guile_procedures();
    console_init_guile_procedures();
    eng_init_guile_procedures();
    guile_init_guile_procedures();
    lisp_init_guile_procedures();
    loop_init_guile_procedures();
    matrix_init_guile_procedures();
    sheet_init_guile_procedures();
    vram_init_guile_procedures();
}

void
init_lisp (const char *main_script)
{
    /* Redirect output/error to console */
    // FIXME: do this

    // Define the Burro module
    scm_c_define_module ("burro", _burroscript_init, NULL);
    
    // Switch to a more useful module and use the Burro module
    SCM burro_user_module = scm_c_define_module ("guile-user", NULL, NULL);
    scm_set_current_module (burro_user_module);

    // Load scheme libraries
    const char *path = g_getenv("BURRO_SCHEME_PATH");
    if (path == NULL)
        path = ".";
    if (g_file_test (path, G_FILE_TEST_IS_DIR) == TRUE)
    {
        SCM old_path = scm_list_copy (scm_c_eval_string ("%load-path"));
        scm_set_car_x (scm_c_eval_string ("%load-path"),
                       scm_from_locale_string (path));
        scm_set_cdr_x (scm_c_eval_string ("%load-path"),
                       old_path);
    }
    
    scm_c_use_module ("burro");
    scm_c_use_module ("rnrs bytevectors");
    scm_c_use_module ("system repl repl");
    scm_c_use_module ("system repl server");
    scm_c_use_module ("system repl coop-server");
    scm_c_use_module ("system vm trap-state");
    
    char *cmd;
    if (main_script)
    {
        lisp_main_script = g_strdup(main_script);
        cmd = g_strdup_printf("(load-from-path \"%s\")", main_script);
    }
    else
    {
        cmd = g_strdup_printf("(load-from-path \"engine.scm\")");
        lisp_main_script = g_strdup("engine.scm");
    }
    scm_c_eval_string(cmd);
    g_free (cmd);
    repl_init ();
}

////////////////////////////////////////////////////////////////
// Here are some random functions for use with the console

SCM_DEFINE (G_restart, "restart", 0, 0, 0, (void), "\
Reload and re-eval the main script.\n")
{
    char *cmd = g_strdup_printf("(load-from-path \"%s\")", lisp_main_script);
    SCM ret = scm_c_eval_string(cmd);
    g_free (cmd);
    return ret;
}

SCM_DEFINE (G_break, "break", 1, 0, 0, (SCM x), "\
Add a breakpoint at a given procedure.")
{
    if (scm_is_true (scm_procedure_p(x)))
    {
        SCM func = scm_c_eval_string("add-trap-at-procedure-call!");
        return scm_call_1(func, x);
    }
    scm_misc_error("break", "~a is not a procedure", scm_list_1 (x));
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (G_list_data_directory_contents, "dir", 0, 1, 0, (SCM regex), "\
List the contents of the data directory.  If a parameter is provided, \n\
it is used as a regular expression for matching.\n")
{
    GError *error = NULL;
    
    const char *assets_dir = g_getenv("BURRO_DATA_DIR");
    const char *user_dir = g_get_user_data_dir ();
    char *path;
    if (assets_dir != NULL)
        path = g_strdup (assets_dir);
    else if (user_dir != NULL)
        path = g_build_path (G_DIR_SEPARATOR_S, user_dir, "burro", NULL);

    GDir *dir = g_dir_open (path, 0, &error);
    if (dir == NULL)
    {
        g_critical ("can't read BURRO data dir: %s", error->message);
        g_error_free (error);
        return SCM_UNSPECIFIED;
    }

    console_write_utf8_string("in ");
    console_write_utf8_string(path);
    console_move_down(1);
    console_move_to_column(0);
    console_write_utf8_string("-----------------------------");
    console_move_down(1);
    console_move_to_column(0);

    const char *fname;
    while ((fname = g_dir_read_name (dir)) != NULL)
    {
        console_write_utf8_string (fname);
        console_move_down(1);
        console_move_to_column(0);
    }

    g_dir_close (dir);
    g_free(path);
    return SCM_UNSPECIFIED;
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
lisp_init_guile_procedures (void)
{
#include "lisp.x"
    scm_c_export("dir", "restart", NULL);
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
