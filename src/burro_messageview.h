#ifndef BURRO_MESSAGEVIEW_H
#define BURRO_MESSAGEVIEW_H

#include <gtk/gtk.h>

#define BURRO_TYPE_MESSASGEVIEW (burro_message_view_get_type ())
G_DECLARE_FINAL_TYPE (BurroMessageView, burro_message_view, BURRO, MESSAGE_VIEW, GtkTextView)

BurroMessageView *burro_message_view_new ();

#endif
