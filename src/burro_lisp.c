#include "x.h"
#include "burro_lisp.h"
#include "burro_app_win.h"
#include "burro_canvas.h"
#include "burro_canvas_vram.h"

/* In this module a couple of things happen.

   1. We create a "burro engine" module, which has all of the
   C-defined engine-specific functions, including idle callbacks,
   mouse click callbacks, backdrop, background, spritesheet,
   textbox, and audio functions.

   2. We load the "burro" module into the (guile-user) environment,
   along with other necessary modules to allow a REPL to be 
   kicked off.  The "burro" module is scheme, but, it
   uses "burro engine".

   3. When we try to open a game file, we make a anonymous sandbox
   environment, which descends from the limited "ice-9 sandbox".
   This anonymous module has the all-pure-and-impure-bindings set,
   plus some of the procedures from the "burro" module.  The
   game file is eval'd in that sandbox.

   Remember to detach all "burro" hook functions before loading a
   new sandbox.
*/

static void
init_burro_engine (void *unused)
{

    // Load up the C-defined procedures
    burro_app_win_init_guile_procedures();
    burro_canvas_init_guile_procedures();
    burro_canvas_vram_init_guile_procedures();
#if 0
    am_init_guile_procedures ();
    backdrop_init_guile_procedures ();
    bg_init_guile_procedures();
    console_init_guile_procedures();
    eng_init_guile_procedures();
    guile_init_guile_procedures();
    lisp_init_guile_procedures();
    loop_init_guile_procedures();
    matrix_init_guile_procedures();
    vram_init_guile_procedures();
    pixbuf_init_guile_procedures();
    obj_init_guile_procedures();
    textbox_init_guile_procedures();

#endif

#if 0
    // Load up scheme coded included as a module
    char *libstr = xg_resources_get_string("/com/lonelycactus/burroengine/burro.scm");
    scm_c_eval_string (libstr);
    g_free(libstr);
#endif
}

static SCM
scm_init_burro_engine_module (void *data)
{
    return scm_c_define_module ("burro engine", init_burro_engine, NULL);
}

static int _last_line_loaded;
static SCM _last_expr_loaded;

static SCM
_load (void *data)
{
    SCM port = SCM_PACK (data);
    _last_line_loaded = -1;
    _last_expr_loaded = SCM_BOOL_F;

    SCM sandbox = scm_c_eval_string ("(define %sandbox (burro-make-new-sandbox))");
  
    while (1) {
        SCM expr = scm_read (port);

        if (scm_is_true (scm_eof_object_p (expr)))
            break;

        if (scm_is_integer (scm_port_line (port)))
            _last_line_loaded = scm_to_int (scm_port_line (port));

        if (scm_is_true (expr))
            _last_expr_loaded = expr;

        scm_eval (expr, scm_current_module());
    }

    return SCM_BOOL_T;
}

static SCM
_open_file (void *data)
{
    SCM s_path = scm_from_locale_string ((char *) data);
    SCM s_mode = scm_from_locale_string ("r0");
    return scm_open_file (s_path, s_mode);
}

static SCM
_default_error_handler (void *data, SCM key, SCM vals)
{
    if (data == NULL)
        return SCM_BOOL_F;
  
    char **err_string = (char **) data;
    char *c_key;
    SCM subr, message, args, rest;
    SCM message_args, formatted_message;
    char *c_message;

    /* Key is the exception type, a symbol. */
    /* exception is a list of 4 elements:
       - subr: a subroutine name (symbol?) or #f
       - message: a format string
       - args: a list of arguments that are tokens for the message
       - rest: the errno, if any */
    if (scm_is_true (key))
        c_key = scm_to_locale_string (scm_symbol_to_string (key));
    else
        c_key = NULL;

    if (c_key && (strcmp (c_key, "unbound-variable") == 0)) {
    
        subr = scm_list_ref (vals, scm_from_int (0));
        message = scm_list_ref (vals, scm_from_int (1));
        args = scm_list_ref (vals, scm_from_int (2));
        rest = scm_list_ref (vals, scm_from_int (3));

        message_args = scm_simple_format (SCM_BOOL_F, message, args);

        if (scm_is_true (subr))
            formatted_message = scm_simple_format (SCM_BOOL_F,
                                                   scm_from_locale_string ("Error ~S: ~A~%"),
                                                   scm_list_2 (subr, message_args));
        else
            formatted_message = scm_simple_format (SCM_BOOL_F,
                                                   scm_from_locale_string ("Error: ~A~%"),
                                                   scm_list_1 (message_args));
    }
    else
        // This is some key for which I don't know the format for the arguments,
        // so I'll just print it out raw.
        formatted_message = scm_simple_format (SCM_BOOL_F,
                                               scm_from_locale_string ("Error ~S: ~S~%"),
                                               scm_list_2 (key, vals));
  


    *err_string = scm_to_locale_string (formatted_message);

    return SCM_BOOL_F;
}


SCM burro_lisp_new ()
{
    SCM burro_user_module = scm_c_define_module ("guile-user", NULL, NULL);
    scm_set_current_module (burro_user_module);

    scm_c_use_module ("ice-9 readline");
    scm_c_use_module ("ice-9 eval-string");
    scm_c_use_module ("ice-9 sandbox");
    scm_c_use_module ("srfi srfi-1");
    scm_c_use_module ("rnrs bytevectors");
    scm_c_use_module ("system repl repl");
    scm_c_use_module ("system repl server");
    scm_c_use_module ("system repl coop-server");
    scm_c_use_module ("system vm trap-state");

    // Loading the internal Burro functions.
    SCM burro_module;
    char *err_string = NULL;
    burro_module = scm_c_catch (SCM_BOOL_T,
                                scm_init_burro_engine_module, NULL,
                                _default_error_handler,
                                (void *) &err_string,
                                NULL, NULL);
    if (scm_is_false (burro_module))
    {
        g_critical(err_string);
        free (err_string);
    }
    else
        scm_c_use_module ("burro");

    return burro_user_module;
}

SCM
burro_make_sandbox (GFile *file, char **err_string)
{
    // Try to parse the file
    SCM sandbox;
    if (file)
    {
        SCM sandbox_func = scm_c_public_ref("burro", "load-file-into-sandbox");
        char *c_path = g_file_get_path (file);
        SCM s_path = scm_from_locale_string (c_path);
        g_free (c_path);
        sandbox = scm_call_1 (sandbox_func, s_path);
    }
    else
    {
        SCM sandbox_func = scm_c_public_ref("burro", "make-sandbox");
        sandbox = scm_call_0(sandbox_func);
    }

    if (scm_is_string (sandbox))
    {
        // The loading failed.  The returned string says why.
        *err_string = scm_to_locale_string (sandbox);
        return SCM_BOOL_F;
    }

    return sandbox;
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
