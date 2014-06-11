#ifdef LIBFAIL_MODE_PERMISSIVE
const int mode = PERMISSIVE;
#define msg g_warning
#else
const int mode = FATAL;
#define msg g_fatal
#endif

pa_mainloop_api *
xpa_mainloop_get_api(pa_mainloop *m)
{
  pa_mainloop_api *a = pa_mainloop_get_api(m);
  if (a == NULL) {
    if (mode == PERMISSIVE)
      g_warning ("pa_mainloop_get_api returns NULL");
    else
      g_fatal ("pa_mainloop_get_api_returns NULL");
  }
  return a;
}

pa_mainloop *xpa_mainloop_new(void)
{
  pa_mainloop *m = pa_mainloop_new();
  if (m == NULL)
    {
      if (mode == PERMISSIVE)
	  g_warning ("pa_mainloop_new returns NULL");
      else
	  g_fatal ("pa_mainloop_new returns NULL");
    }
  return m;
}

