#include "engine.h"
#include "eng_state.h"

void initialize_state(void)
{
    bg_entry_t bacg;
    e.priv.initialized_flag = FALSE;
    e.priv.minimized_flag = FALSE;
    e.priv.active_flag = FALSE;
    e.priv.quitting_flag = FALSE;

    /* Set up the frame count timer */
    e.priv.fps_timer = g_timer_new();
    g_timer_start (e.priv.fps_timer);
    e.priv.frame_count = 0;
    e.priv.prev_time = g_timer_elapsed (e.priv.fps_timer, NULL);
    e.priv.cur_time =  g_timer_elapsed (e.priv.fps_timer, NULL);

}

