/* loop.cpp -- the main loop

   Copyright 2014, Michael L. Gran

   This file is part of the Project Burro game engine.

   Project Burro is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   Project Burro is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Project Burro.  If not, see
   <http://www.gnu.org/licenses/>. */


#include <cstdint>
#include <SDL.h>
#include "jsapi.hpp"
#include "Interpreter.hpp"
#include "loop.hpp"
#include "console.hpp"
#include "draw.hpp"
#include "utf.hpp"
#include "lineedit.hpp"
#include "eng.hpp"
#include "const.h"

static void paint ();

// The main loop structure
Loop loop {};

Loop::Loop()
	: active_flag{false},
	console_flag{false},
	autocomplete_flag{false},
	initialized_flag{false},
	minimized_flag{false},
	quitting_flag{false},
	run_full_speed_flag{true},
	update_count{0},
	draw_count{0},
	before_update_time{0},
	after_update_time{0},
	before_draw_time{0},
	after_draw_time{0},
	hundred_frames_draw_time{0},
	measured_frame_rate{0.0}
{
}

void Loop::set_console_mode (bool flag)
{
    if (flag == true) {
        console_flag = true;
        SDL_StartTextInput();
        lineedit_start(buf, 200, L"->");
    }
    else {
        console_flag = false;
        SDL_StopTextInput();
        memset(buf, 0, sizeof(buf));
    }
}

void Loop::go ()
{
    active_flag = true;
  
    update_count = 0;
    draw_count = 0;
  
    before_update_time = SDL_GetTicks ();
    after_update_time = before_update_time;
    before_draw_time = before_update_time;
    after_draw_time = before_update_time;
  
    initialized_flag = true;
  
    while (1) {    
        SDL_Event e;
		
        if (SDL_PollEvent(&e)) {
            if (console_flag && (e.type == SDL_KEYDOWN || e.type == SDL_TEXTINPUT)) {
                do_console_event(e);
                continue;
            }
			
            switch(e.type) {
			case SDL_QUIT:
				return;
            default:
                on_idle();
                break;
            }
        }
        else
            on_idle();
        if (!run_full_speed_flag)
            SDL_Delay(1);
    }  
}

void Loop::do_console_event (SDL_Event e)
{
    switch(e.type) {
    case SDL_QUIT:
        break;
    case SDL_KEYDOWN:
        if (autocomplete_flag)
        {
            ;
        }
        else {
            // Here we process non-textual keys
            if (e.key.keysym.sym == SDLK_BACKSPACE)
                lineedit_backspace();
            if (e.key.keysym.sym == SDLK_TAB)
                lineedit_autocomplete();
            if (e.key.keysym.mod == KMOD_LCTRL || e.key.keysym.mod == KMOD_RCTRL)
            {
                if (e.key.keysym.sym == SDLK_a)
                    lineedit_move_home();
                if (e.key.keysym.sym == SDLK_b)
                    lineedit_move_left();
                if (e.key.keysym.sym == SDLK_c)
                    lineedit_ctrl_c();
                if (e.key.keysym.sym == SDLK_d)
                    lineedit_delete_or_eof();
                if (e.key.keysym.sym == SDLK_e)
                    lineedit_move_end();
                if (e.key.keysym.sym == SDLK_f)
                    lineedit_move_right();
                if (e.key.keysym.sym == SDLK_h)
                    lineedit_backspace();
                if (e.key.keysym.sym == SDLK_k)
                    lineedit_delete_to_end();
                if (e.key.keysym.sym == SDLK_l)
                    lineedit_clear_screen();
                if (e.key.keysym.sym == SDLK_n)
                    lineedit_history_next();
                if (e.key.keysym.sym == SDLK_p)
                    lineedit_history_prev();
                if (e.key.keysym.sym == SDLK_t)
                    lineedit_swap_chars();
                if (e.key.keysym.sym == SDLK_u)
                    lineedit_delete_line();
                if (e.key.keysym.sym == SDLK_w)
                    lineedit_delete_word_prev();
            }
            if (e.key.keysym.scancode == SDL_SCANCODE_DELETE)
                lineedit_delete();
            if (e.key.keysym.scancode == SDL_SCANCODE_DOWN)
                lineedit_history_next();
            if (e.key.keysym.scancode == SDL_SCANCODE_END)
                lineedit_move_end();
            if (e.key.keysym.scancode == SDL_SCANCODE_HOME)
                lineedit_move_home();
            if (e.key.keysym.scancode == SDL_SCANCODE_LEFT)
                lineedit_move_left();
            if (e.key.keysym.scancode == SDL_SCANCODE_RIGHT)
                lineedit_move_right();
            if (e.key.keysym.scancode == SDL_SCANCODE_UP)
                lineedit_history_prev();
            else if (e.key.keysym.sym == SDLK_TAB)
                ;
            else if (e.key.keysym.sym == SDLK_CLEAR)
                ;
            else if (e.key.keysym.sym == SDLK_RETURN)
                ;
            else if (e.key.keysym.sym == SDLK_PAUSE)
                ;
            else if (e.key.keysym.sym == SDLK_DELETE)
            {
            }
            if (e.key.keysym.scancode == SDL_SCANCODE_RETURN) {
                // End this lineedit session
                // Act on the string
                // Maybe add the string to the history
                lineedit_stop();
                console_move_to_column(0);
                console_move_down(1);
                {
                    char *script = lineedit_get_text();
                    if (strlen(script) > 0) {
                        interpreter.Do_console_command(script);
                    }
                }
                lineedit_start(buf, 200, L"->");
            }
        }
        break;
    case SDL_TEXTEDITING:
        // printf("textedit %s\n", e.edit.text);
        break;
    case SDL_TEXTINPUT:
        // printf("textinput %s\n", e.text.text);
        // SDL text is always UTF-8.  Convert to wchar_t.
        do {
            wchar_t *input = utf8_to_wcs_alloc (e.text.text);
            if (input != NULL) {
                if (autocomplete_flag)
                    lineedit_autocomplete_text_input(input);
                else
                    lineedit_text_input(input);
                free(input);
            }
        } while (0);
        break;
    default:
        on_idle();
        break;
    }
}

bool Loop::on_idle (void)
{
    if (quitting_flag)
        return false;

    uint32_t cur_time = SDL_GetTicks ();

    if (initialized_flag && !minimized_flag)
    {
        if (active_flag)
        {
            //audio_time_cur = cur_time;
            //audio_update();

            if (run_full_speed_flag || ((cur_time - before_update_time) > UPDATE_RATE))
            {
				// All the game logic happens in this call.
                interpreter.Do_idle (cur_time - before_update_time);

                update_count ++;
                before_update_time = cur_time;
                after_update_time = SDL_GetTicks();

                if (run_full_speed_flag || ((cur_time - before_draw_time) > REFRESH_RATE))
                {
                    paint ();
                    
                    interpreter.Do_after_draw_frame(after_draw_time - before_draw_time);
		  
                    draw_count ++;
                    before_draw_time = cur_time;
                    after_draw_time = SDL_GetTicks ();
		  
                    if (draw_count % 100 == 0)
                    {
                        uint32_t microsecs_for_100_frames = after_draw_time - hundred_frames_draw_time;
                        double secs_per_frame = 0.001 * 0.01 * (double) microsecs_for_100_frames;
                        hundred_frames_draw_time = after_draw_time;
                        SDL_LogDebug (SDL_LOG_CATEGORY_APPLICATION,
                                      "Frame Rate: %f fps\n",
                                      1.0 / secs_per_frame);
                    }
                }
            }

        }
        else
        {
            //audio_pause ();
            // This really ought to wait for when a new window event
            // happens.
        }
    }

    return true;
}

static void paint ()
{
    /* Have the engine render the backgrounds and objects to a bitmap */
    draw ();

    /* Have the bitmap pushed onto the screen. */
    eng_present ();
}

static JSBool
LoopDieFunc(JSContext *cx, unsigned argc, jsval *vp)
{
	SDL_Event user_event;

	user_event.type = SDL_QUIT;
	
	SDL_PushEvent(&user_event);
    JS_SET_RVAL(cx, vp, JSVAL_VOID);
	return JS_TRUE;
}

JSFunctionSpec game_loop_functions[2] = {
    JS_FS("die", LoopDieFunc, 0, 0),
    JS_FS_END
};

