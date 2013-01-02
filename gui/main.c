#include <gtk/gtk.h>
#include "engine.h"
#ifdef GAME
#include "game.h"
#endif


static void log_handler (const gchar *log_domain,
                         GLogLevelFlags log_level,
                         const gchar *message,
                         gpointer user_data)
{
    printf("%s\n", message);
}

int main(int argc, char **argv)
{
    int i;

    // g_log_set_handler (NULL, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION, log_handler, NULL);

    engine_initialize(&argc, &argv, "Project Burro");

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
