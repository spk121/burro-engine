#include <libguile.h>
#include "burro_config_keys.h"
#include "burro_repl.h"

static SCM
eval_string_catch_handler (const char *string, SCM key, SCM args)
{
    scm_write(key, scm_current_error_port());
    scm_write(args, scm_current_error_port());
    g_critical ("scm_c_eval_string of \"%s\" failed", string);
    return SCM_BOOL_F;
}

static SCM
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

struct _BurroRepl
{
  GObject    parent;
  gboolean   initialized;
  gboolean   enabled;
  gboolean   continuing;
  SCM        server_socket;
  SCM        server;
  SCM        poll_func;
  guint      idle_event_source_id;
};

G_DEFINE_TYPE (BurroRepl, burro_repl, G_TYPE_OBJECT)

static
gboolean idle_cb (gpointer data)
{
  BurroRepl *repl = BURRO_REPL (data);

  if (!repl->enabled)
    return repl->continuing;
  
  if (!repl->initialized)
    {
      SCM spawn_func;
      repl->server_socket = xscm_c_eval_string("(make-tcp-server-socket #:port 37147)");
      spawn_func = xscm_c_eval_string("spawn-coop-repl-server");
      repl->poll_func = xscm_c_eval_string ("poll-coop-repl-server");
      if (scm_is_false (repl->server_socket)
	  || scm_is_false (spawn_func)
	  || scm_is_false (repl->poll_func))
	{
	  g_critical ("REPL is broken.  Disabling REPL setting.");
	  repl->continuing = G_SOURCE_REMOVE;
	  return repl->continuing;
	}
      repl->server = scm_call_1 (spawn_func, repl->server_socket);
      repl->initialized = TRUE;
    }
  scm_call_1 (repl->poll_func, repl->server);

  return repl->continuing;
}


static void
burro_repl_class_init (BurroReplClass *class)
{
  
}

static void
burro_repl_init (BurroRepl *self)
{
  self->initialized        = FALSE;
  self->enabled            = FALSE;
  self->server_socket      = SCM_UNSPECIFIED;
  self->server             = SCM_UNSPECIFIED;
  self->poll_func          = SCM_UNSPECIFIED;
  self->continuing         = G_SOURCE_CONTINUE;
  self->idle_event_source_id = 
    g_timeout_add_full (G_PRIORITY_DEFAULT_IDLE,
			10,
			idle_cb,
			(gpointer) self,
			NULL);
  
}

static void
burro_repl_dispose (GObject *object)
{
  BurroRepl *repl = BURRO_REPL (object);
  repl->continuing = G_SOURCE_REMOVE;
  G_OBJECT_CLASS (burro_repl_parent_class)->dispose (object);
}

static void
burro_repl_finalize (GObject *object)
{
  BurroRepl *self = BURRO_REPL (object);
  
  self->initialized   = FALSE;
  self->server_socket = SCM_UNSPECIFIED;
  self->server        = SCM_UNSPECIFIED;
  self->poll_func     = SCM_UNSPECIFIED;
  
  G_OBJECT_CLASS (burro_repl_parent_class)->finalize (object);
}

void
burro_repl_enable (BurroRepl *repl)
{
  repl->enabled = TRUE;
}
