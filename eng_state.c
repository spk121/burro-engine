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
    e.priv.timer = g_timer_new();
    g_timer_start (e.priv.timer);
    e.priv.update_count = 0;
    e.priv.draw_count = 0;
    e.priv.before_update_time = g_timer_elapsed (e.priv.timer, NULL);
    e.priv.after_update_time = e.priv.before_update_time;
    e.priv.before_draw_time =  e.priv.before_update_time;
    e.priv.after_draw_time =  e.priv.before_update_time;

    e.brightness = 1.0;
}

