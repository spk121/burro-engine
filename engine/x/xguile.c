#include <glib.h>
#include "xguile.h"

static SCM
eval_string_catch_handler (const char *string, SCM key, SCM args)
{
    scm_write(key, scm_current_error_port());
    scm_write(args, scm_current_error_port());
    g_error ("scm_c_eval_string of \"%s\" failed", string);
    return SCM_BOOL_F;
}

static SCM
eval_string_catch_handler_warn (const char *string, SCM key, SCM args)
{
    scm_write(key, scm_current_error_port());
    scm_write(args, scm_current_error_port());
    g_warning ("scm_c_eval_string of \"%s\" failed", string);
    return SCM_BOOL_F;
}

static SCM
primitive_load_catch_handler (const gchar *filename, SCM key, SCM args)
{
    // Sometimes, for testing, I use scm_c_primitive_load to load
    // a script that has an exit call in it.
    if (scm_is_eq (key, scm_from_latin1_symbol("quit")))
    {
        g_debug ("scm_c_primitive_load of %s has caused a quit", filename);
        
        exit (scm_to_int (scm_car (args)));
    }
    g_error ("scm_c_primitive_load of %s failed", filename);
    return SCM_BOOL_F;
}

SCM
xscm_c_eval_string (const gchar *string)
{
    g_return_val_if_fail (string != NULL && strlen (string) > 0, SCM_BOOL_F);
    return scm_c_catch (SCM_BOOL_T,
                        (scm_t_catch_body) scm_c_eval_string,
                        (void *) string,
                        (scm_t_catch_handler) eval_string_catch_handler,
                        (void *) string,
                        (scm_t_catch_handler) NULL,
                        (void *) NULL);
}

SCM
xscm_c_eval_string_or_warn (const gchar *string)
{
    g_return_val_if_fail (string != NULL && strlen (string) > 0, SCM_BOOL_F);
    return scm_c_catch (SCM_BOOL_T,
                        (scm_t_catch_body) scm_c_eval_string,
                        (void *) string,
                        (scm_t_catch_handler) eval_string_catch_handler_warn,
                        (void *) string,
                        (scm_t_catch_handler) NULL,
                        (void *) NULL);
}

SCM
xscm_c_primitive_load (const gchar *filename)
{
    g_return_val_if_fail (filename != NULL && strlen (filename) > 0, SCM_BOOL_F);
    
    return scm_c_catch (SCM_BOOL_T,
                        (scm_t_catch_body) scm_c_primitive_load,
                        (void *) filename,
                        (scm_t_catch_handler) primitive_load_catch_handler,
                        (void *) filename,
                        (scm_t_catch_handler) NULL,
                        (void *) NULL);
}

SCM
xscm_from_latin1_symbol (const char *name)
{
    g_return_val_if_fail (name != NULL, SCM_BOOL_F);
    return scm_from_latin1_symbol (name);
}

/* A simple error handler that returns false when an error occurs.  */
static SCM
_xscm_false_error_handler (void *data, SCM key, SCM exception)
{
    return SCM_BOOL_F;
}

#if 0
static SCM
_xscm_lookup_safe_body (void *data)
{
    return scm_lookup (SCM_PACK (data));
}
#endif

static SCM
_xscm_c_resolve_module_safe_body (void *data)
{
    return scm_c_resolve_module ((const char *) data);
}

SCM
xscm_c_resolve_module (const char *name)
{
    SCM ret = scm_c_catch (SCM_BOOL_T,
                           _xscm_c_resolve_module_safe_body, (void *) name,
                           _xscm_false_error_handler, (void *) name,
                           NULL, NULL);
    return ret;
}

bool
xscm_is_symbol(SCM x)
{
    return scm_is_true (scm_symbol_p (x));
}

bool
xscm_is_variable(SCM x)
{
    return scm_is_true (scm_variable_p (x));
}

bool
xscm_is_procedure(SCM x)
{
    return scm_is_true (scm_procedure_p (x));
}

int
xscm_val_to_int (SCM x)
{
    if (SCM_UNBNDP (x))
        return 0;
    else if (scm_is_bool (x))
    {
        if (scm_is_false(x))
            return 0;
        else
            return 1;
    }
    else if (scm_is_integer (x))
        return scm_to_int (x);

    return 0;
}
        

void
xscm_init_guile (void)
{
    scm_init_guile ();
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

