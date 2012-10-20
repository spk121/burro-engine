#include <gtk/gtk.h>
#include "engine.h"
#include "game.h"

int main()
{
    int i;

    g_log_set_handler ("Gtk", G_LOG_LEVEL_WARNING, (GLogFunc) gtk_false, NULL);
    gtk_init (0, NULL);
    //gst_init (0, NULL);
    g_log_set_handler (NULL, G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION, g_log_default_handler, NULL);

    /* Initialize the engine */
    eng_init ();

    // game_init ();

    /* Hook the game into the engine */
    //e.do_before_draw_frame = game_update;
    e.do_idle = NULL;
    e.do_before_draw_frame = NULL;
    e.do_after_keypress = NULL;
    for (i = 0; i < TIMER_COUNT; i ++)
        e.do_after_timer[i] = NULL;

    /* Main loop */
    eng_main ();

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
