#include <glib.h>
#include <libguile.h>
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
primitive_load_catch_handler (const gchar *filename, SCM key, SCM args)
{
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
xscm_c_primitive_load (const gchar *filename)
{
    g_return_val_if_fail (filename != NULL && strlen (filename) > 0, SCM_BOOL_F);
    
    /* FIXME: catch Guile errors here */
    return scm_c_catch (SCM_BOOL_T,
                        (scm_t_catch_body) scm_c_primitive_load,
                        (void *) filename,
                        (scm_t_catch_handler) primitive_load_catch_handler,
                        (void *) filename,
                        (scm_t_catch_handler) NULL,
                        (void *) NULL);
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

