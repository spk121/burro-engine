#include <gtk/gtk.h>
#include "engine.h"
#ifndef NO_GAME
#include "game.h"
#endif

int main(int argc, char **argv)
{
    int i;

    g_log_set_handler ("Gtk", G_LOG_LEVEL_WARNING, (GLogFunc) gtk_false, NULL);
    g_log_set_handler (NULL, G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION, g_log_default_handler, NULL);

#ifndef NO_GAME
    /* Initialize the engine */
    engine_initialize(&argc, &argv, game_get_title());
    game_initialize (&argc, &argv, e);
#else
    engine_initialize(&argc, &argv, "Project Burro");
#endif

    /* Main loop */
    engine_loop();

    return 0;
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
