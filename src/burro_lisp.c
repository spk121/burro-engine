#include "burro_lisp.h"

static void
_burroscript_init (void *unused)
{
  // Start with a limited scheme vocabularity, for safety.
  scm_c_use_module ("ice-9 safe");

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

    char *libstr = xg_resources_get_string("/com/lonelycactus/burroengine/library.scm");
    xscm_c_eval_string_or_warn (libstr);
    g_free(libstr);
#endif
}

static SCM
_define_module (void *data)
{
  return scm_c_define_module ("burro", _burroscript_init, NULL);
}

static int _last_line_loaded;
static SCM _last_expr_loaded;

static SCM
_load (void *data)
{
  SCM port = SCM_PACK (data);
  _last_line_loaded = -1;
  _last_expr_loaded = SCM_BOOL_F;
  
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

gboolean
burro_lisp_load (SCM module, GFile *file, char **err_string)
{
  scm_set_current_module (module);

  if (!file)
    {
      *err_string = g_strdup("No file specified");
      return FALSE;
    }

  // Next, try to parse the file
  SCM ret;
#if 0
  char *contents;
  gsize length;
  GError *error = NULL;
  gboolean ok = g_file_load_contents (file,
				      NULL,
				      &contents, &length,
				      NULL,
				      &error);
  
  if (!ok)
    {
      if (err_string)
	*err_string = g_strdup_printf ("Unable to read file: %s",
				       error->message);
      g_error_free (error);
      g_free (contents);
      return FALSE;
    }

  ret = 
    scm_c_catch (SCM_BOOL_T,
		 (scm_t_catch_body) scm_c_eval_string,
		 (void *) contents,
		 (scm_t_catch_handler) _default_error_handler,
		 (void *) err_string,
		 (scm_t_catch_handler) NULL,
		 (void *) NULL);

  g_free (contents);
#else
  // I'm writing a crippled scheme parser using the C API?  What is
  // wrong with me?
  char *c_path = g_file_get_path (file);
  SCM port =
    scm_c_catch (SCM_BOOL_T,
		 _open_file,
		 (void *) c_path,
		 _default_error_handler,
		 (void *) err_string,
		 NULL,
		 NULL);
  g_free (c_path);
  if (scm_is_false (port))
    return FALSE;
  
  ret = 
    scm_c_catch (SCM_BOOL_T,
		 (scm_t_catch_body) _load,
		 (void *) SCM_UNPACK (port),
		 (scm_t_catch_handler) _default_error_handler,
		 (void *) err_string,
		 (scm_t_catch_handler) NULL,
		 (void *) NULL);
#endif
  if (scm_is_false (ret))
    {
      if (_last_line_loaded >= 0)
	{
	  SCM format_str = scm_from_locale_string ("~A");
	  SCM expr_string = scm_simple_format (SCM_BOOL_F,
					       format_str,
					       scm_list_1 (_last_expr_loaded));
	  char *c_expr_string = scm_to_locale_string (expr_string);
	  
	  char *new_err_string = g_strdup_printf("%s\nIn expression:\n%s\n\nAround line #%d",
						 *err_string,
						 c_expr_string,
						 _last_line_loaded);
	  g_free (*err_string);
	  free (c_expr_string);
	  *err_string = new_err_string;
	}
      return FALSE;
    }

  return TRUE;
}

SCM burro_lisp_new (char **err_string)
{
  // First, try loading the internal Burro functions.
  SCM module;
  module = scm_c_catch (SCM_BOOL_T,
			_define_module, NULL,
			_default_error_handler,
			(void *) err_string,
			NULL, NULL);
  if (scm_is_false (module))
      return SCM_BOOL_F;

  return module;
}

