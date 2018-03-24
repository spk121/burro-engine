#ifndef BURRO_REPL_H
#define BURRO_REPL_H
#include <gtk/gtk.h>
#include "burro_app.h"

#define BURRO_TYPE_REPL (burro_repl_get_type ())
G_DECLARE_FINAL_TYPE (BurroRepl, burro_repl, BURRO, REPL, GObject)

BurroRepl *burro_repl_new ();

#endif
