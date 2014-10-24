#include <glib.h>
#include "xpulseaudio.h"

static gboolean
is_valid_pa_context_state_t (pa_context_state_t c)
{
  return (c == PA_CONTEXT_UNCONNECTED
	  || c == PA_CONTEXT_CONNECTING
	  || c == PA_CONTEXT_AUTHORIZING
	  || c == PA_CONTEXT_SETTING_NAME
	  || c == PA_CONTEXT_READY
	  || c == PA_CONTEXT_FAILED
	  || c == PA_CONTEXT_TERMINATED);
}

void
xpa_context_connect_to_default_server (pa_context *c)
{
  int ret;
  ret = pa_context_connect (c, (const char *) NULL, PA_CONTEXT_NOFLAGS, (const pa_spawn_api *) NULL);
  if (ret != 0)
    g_critical ("pa_context_connect failed");
}

pa_context_state_t
xpa_context_get_state (pa_context *c)
{
  pa_context_state_t s;
  g_return_val_if_fail (c != NULL, PA_CONTEXT_UNCONNECTED);
  s = pa_context_get_state (c);
  g_return_val_if_fail (is_valid_pa_context_state_t (s), s);
  return s;
}

pa_context *
xpa_context_new_with_proplist (pa_mainloop_api *mainloop, const char *name, pa_proplist *proplist)
{
  pa_context *c;
  c = pa_context_new_with_proplist (mainloop, name, proplist);
  if (c == NULL)
    g_critical ("pa_context_new_with_proplist returned NULL");
  return c;
}

void
xpa_context_set_state_callback (pa_context *c, pa_context_notify_cb_t cb, void *userdata)
{
  g_return_if_fail (c != NULL);
  g_return_if_fail (cb != NULL);
  pa_context_set_state_callback (c, cb, userdata);
}

pa_mainloop_api *
xpa_mainloop_get_api (pa_mainloop *m)
{
  pa_mainloop_api *api;
  g_return_val_if_fail (m != NULL, NULL);
  api = pa_mainloop_get_api (m);
  if (api == NULL)
    g_critical ("pa_mainloop_get_api returned NULL");
  return api;
}

void
xpa_mainloop_blocking_iterate (pa_mainloop * m)
{
  int ret;
  g_return_if_fail (m != NULL);
  ret = pa_mainloop_iterate (m, 1, NULL);
  if (ret < 0)
    g_critical ("pa_mainloop_iterate did not succeed");
}

int
xpa_mainloop_nonblocking_iterate (pa_mainloop * m)
{
  int ret;
  g_return_val_if_fail (m != NULL, -1);
  ret = pa_mainloop_iterate (m, 0, NULL);
  if (ret < 0)
    g_critical ("pa_mainloop_iterate did not succeed");
  return ret;
}


void
xpa_mainloop_free (pa_mainloop *m)
{
  g_return_if_fail (m != NULL);
  pa_mainloop_free (m);
}

pa_mainloop *
xpa_mainloop_new (void)
{
  pa_mainloop *ml = pa_mainloop_new ();
  if (ml == NULL)
    g_critical ("pa_mainloop_new returned NULL");
  return ml;
}

void
xpa_proplist_free (pa_proplist *p)
{
  g_return_if_fail (p != NULL);
  pa_proplist_free (p);
}

pa_proplist *
xpa_proplist_new (void)
{
  pa_proplist *p;
  p = pa_proplist_new ();
  if (p == NULL)
    g_critical ("pa_proplist_new returned NULL");
  return p;
}

void
xpa_proplist_sets (pa_proplist *p, const char *key, const char *value)
{
  int ret;
  ret = pa_proplist_sets (p, key, value);
  if (ret != 0)
    g_critical ("pa_proplist_sets failed");
}

void xpa_stream_connect_playback_to_default_device (pa_stream *s, const pa_buffer_attr *attr, 
						    pa_stream_flags_t flags)
{
  int ret;
  g_return_if_fail (s != NULL);
  ret = pa_stream_connect_playback (s, NULL, attr, flags, NULL, NULL);
  if (ret != 0)
    g_critical ("pa_stream_connect_playback failed");
}

pa_stream *
xpa_stream_new_with_proplist (pa_context *c, const char *name, const pa_sample_spec *ss, 
			      const pa_channel_map *map, pa_proplist *p)
{
  pa_stream *s;
  g_return_val_if_fail (c != NULL, NULL);
  g_return_val_if_fail (name != NULL, NULL);
  g_return_val_if_fail (ss != NULL, NULL);
  g_return_val_if_fail (map != NULL, NULL);
  g_return_val_if_fail (p != NULL, NULL);
  s = pa_stream_new_with_proplist (c, name, ss, map, p);
  if (s == NULL)
    g_critical ("pa_stream_new_with_proplist returned NULL");
  return s;
}

void
xpa_stream_set_started_callback (pa_stream *p, pa_stream_notify_cb_t cb, void *userdata)
{
  g_return_if_fail (p != NULL);
  g_return_if_fail (cb != NULL);
  pa_stream_set_started_callback (p, cb, userdata);
}

void
xpa_stream_set_write_callback (pa_stream *p, pa_stream_request_cb_t cb, void *userdata)
{
  g_return_if_fail (p != NULL);
  g_return_if_fail (cb != NULL);
  pa_stream_set_write_callback (p, cb, userdata);
}

void
xpa_stream_write (pa_stream *p, const void *data, size_t nbytes)
{
  int ret;
  ret = pa_stream_write (p, data, nbytes, NULL, 0, PA_SEEK_RELATIVE);
  if (ret != 0)
    g_critical ("pa_stream_write failed");
}

