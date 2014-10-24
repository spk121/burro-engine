#include "engine.h"
#include "eng_state.h"

void initialize_state(void)
{
    e.priv.initialized_flag = FALSE;
    e.priv.minimized_flag = FALSE;
    e.priv.active_flag = FALSE;
    e.priv.quitting_flag = FALSE;

    e.brightness = 1.0;
}

