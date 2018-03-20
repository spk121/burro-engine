#ifndef BURRO_XPULSEAUDIO_H
#define BURRO_XPULSEAUDIO_H

#include <glib.h>
#include <pulse/pulseaudio.h>
#include <pulse/glib-mainloop.h>
void                xpa_context_connect_to_default_server (pa_context *c);
pa_context_state_t  xpa_context_get_state              (pa_context *c);
pa_context *        xpa_context_new_with_proplist      (pa_mainloop_api *mainloop,
                                                        const char *name,
                                                        pa_proplist *proplist);
void                xpa_context_set_state_callback     (pa_context *c,
							pa_context_notify_cb_t cb,
							void *userdata);
pa_glib_mainloop *   xpa_glib_mainloop_new             (GMainContext* c);
void                xpa_mainloop_free                  (pa_mainloop *m);
pa_mainloop_api *   xpa_mainloop_get_api               (pa_mainloop *m);
void                xpa_mainloop_blocking_iterate      (pa_mainloop * m);
int                 xpa_mainloop_nonblocking_iterate   (pa_mainloop * m);
pa_mainloop *       xpa_mainloop_new                   (void);
void                xpa_proplist_free                  (pa_proplist *p);
pa_proplist *       xpa_proplist_new                   (void);
void                xpa_proplist_sets                  (pa_proplist *p,
                                                        const char *key,
                                                        const char *value);
void                xpa_stream_connect_playback_to_default_device (pa_stream *s,
                                                                   pa_context *c,
                                                                   const pa_buffer_attr *attr,
                                                                   pa_stream_flags_t flags);
pa_usec_t           xpa_stream_get_time                (pa_stream *s);

pa_stream *         xpa_stream_new_with_proplist       (pa_context *c,
							const char *name,
							const pa_sample_spec *ss,
							const pa_channel_map *map,
							pa_proplist *p);
void                xpa_stream_set_started_callback     (pa_stream *p,
							 pa_stream_notify_cb_t cb,
							 void *userdata);
void                xpa_stream_set_write_callback       (pa_stream *p,
							 pa_stream_request_cb_t cb,
							 void *userdata);
void                xpa_stream_write                   (pa_stream * p,
                                                        const void *data,
                                                        size_t nbytes);
#endif
