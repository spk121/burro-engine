#include <glib.h>
#include <libguile.h>
#include "xguile.h"

static SCM
eval_string_catch_handler (const gchar *string, SCM key, SCM args)
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
                      scm_c_eval_string,
                      string,
                        eval_string_catch_handler,
                        string,
                        NULL, NULL);
}

SCM
xscm_c_primitive_load (const gchar *filename)
{
    g_return_val_if_fail (filename != NULL && strlen (filename) > 0, SCM_BOOL_F);
    
    /* FIXME: catch Guile errors here */
    return scm_c_catch (SCM_BOOL_T,
                        scm_c_primitive_load,
                        filename,
                        primitive_load_catch_handler,
                        filename,
                        NULL, NULL);
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

