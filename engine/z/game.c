#include <glib.h>
#include <stdlib.h>
#include "../y/bg.h"
#include "../y/loop.h"
#include "../y/rand.h"
#include "../y/proc.h"
#include "game.h"

void game_initialize (void)
{
  /* Setup splash background */
  bg_set_backdrop_color (BG_COLOR16_BLACK);

  /* Punt to splash screen task */
  // GHook *splash = proc_splash_new ();
  // pm_queue_proc (splash);

  bg_set_bmp16_from_resource (BG_MAIN_0, "/com/lonelycactus/burro/splash_bmp16.tga");
  bg_set (BG_MAIN_0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
  bg_set_priority (BG_MAIN_0, 0);
  bg_show (BG_MAIN_0);

  /* Timer for menu activation */
  // GHook *wait = proc_wait_new (3.0);
  // pm_queue_proc (wait);

#if 0
  /* Load intro music */
  proc_tune_t *tune = tune_load ("splash");

  /* Queue into music */
  queue_proc (TOP_LEVEL, TUNE, tune);

  proc_menu_t *menu = proc_menu_make_splash_menu ();

  /* Append menu control to timer */
  queue_proc (wait, MENU, menu);
  queue_proc (TOP_LEVEL, WAIT, wait);
#endif

  loop_set_game_update_func (game_update);
}

int game_update (double delta_t)
{
  static double t = 0.0;
  static guint16 c = 0;

  // g_hook_list_invoke_check (delta_t);

  t += delta_t;
  if (t > 10.0)
    {
      c = rand_int_range (0, 0x7fff);
      t = 0.0;
    }
  else
      c ++;
  bg_set_backdrop_color (c);

#if 0
  guint n;
  n = proc_iterate ();
  if (n == 0)
    game_shutdown ();
#endif

  return 1;
}
