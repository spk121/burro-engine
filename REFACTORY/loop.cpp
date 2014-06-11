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

#include <stdbool.h>
#include <stdint.h>
#include <SDL.h>
#include "jsapi.hpp"
#include "js.hpp"
#include "loop.hpp"
#include "js.hpp"
#include "console.hpp"
#include "draw.hpp"
#include "utf.hpp"
#include "lineedit.hpp"
#include "eng.hpp"
#include "const.h"


bool active_flag = false;
bool console_flag = false;     /* true when in text input mode */
bool autocomplete_flag = false; /* true when trying to tab-complete at the console */
bool initialized_flag = false;
bool minimized_flag = false;
bool quitting_flag = false;
bool run_full_speed_flag = false;

int update_count = 0;
int draw_count = 0;

uint32_t timer = 0;
uint32_t before_update_time = 0;
uint32_t after_update_time = 0;
uint32_t before_draw_time = 0;
uint32_t after_draw_time = 0;
uint32_t hundred_frames_draw_time = 0;
double measured_frame_rate = 0.0;

static wchar_t buf[200];

static void do_console_event (SDL_Event e);
static bool on_idle (void);
static void paint (void);

void loop_set_console_mode (bool flag)
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


void loop ()
{
    active_flag = true;
  
    timer = SDL_GetTicks ();
  
    update_count = 0;
    draw_count = 0;
  
    before_update_time = timer;
    after_update_time = before_update_time;
    before_draw_time = before_update_time;
    after_draw_time = before_update_time;
  
    initialized_flag = true;
  
    while (1) {    
        SDL_Event e;
        // bg_set_backdrop_color(before_update_time);
        if (SDL_PollEvent(&e)) {
            if (console_flag && (e.type == SDL_KEYDOWN || e.type == SDL_TEXTINPUT)) {
                do_console_event(e);
                continue;
            }

            switch(e.type) {
            case SDL_QUIT:
                break;
            case SDL_KEYDOWN:
                printf("%u %u %d %d %d %d %d %s\n", e.key.timestamp, e.key.windowID,
                       (int)e.key.state, (int)e.key.repeat,
                       (int)e.key.keysym.sym, e.key.keysym.scancode, e.key.keysym.mod,
                       SDL_GetKeyName(e.key.keysym.sym));
                if (e.key.keysym.sym == SDLK_RETURN) {
                    console_move_to_column(0);
                    console_move_down(1);
                }
	  
                break;
            case SDL_TEXTEDITING:
                printf("textedit %s\n", e.edit.text);
                break;
            case SDL_TEXTINPUT:
                printf("textinput %s\n", e.text.text);
                console_write_utf8_string(e.text.text);
                break;
            default:
                on_idle();
                break;
            }
        }
        else
            on_idle();
        SDL_Delay(10);
    }  
}

static void
do_console_event (SDL_Event e)
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
            // backspace ->  8 42 0
            if (e.key.keysym.sym == SDLK_BACKSPACE)
                lineedit_backspace();
            if (e.key.keysym.sym == SDLK_TAB)
                lineedit_autocomplete();
            if (e.key.keysym.mod == KMOD_LCTRL || e.key.keysym.mod == KMOD_RCTRL)
            {
                // ctrl-a -> 97 4 64
                if (e.key.keysym.sym == SDLK_a)
                    lineedit_move_home();
                // ctrl-b ->  98 5 64
                if (e.key.keysym.sym == SDLK_b)
                    lineedit_move_left();
                // ctrl-c ->  99 6 64
                if (e.key.keysym.sym == SDLK_c)
                    lineedit_ctrl_c();
                // ctrl-d ->  100 7 64
                if (e.key.keysym.sym == SDLK_d)
                    lineedit_delete_or_eof();
                // ctrl-e -> 101 8 64
                if (e.key.keysym.sym == SDLK_e)
                    lineedit_move_end();
                // ctrl-f ->   102 9 64
                if (e.key.keysym.sym == SDLK_f)
                    lineedit_move_right();
                // ctrl-h ->  104 11 64
                if (e.key.keysym.sym == SDLK_h)
                    lineedit_backspace();
                // ctrl-k -> 107 14 64
                if (e.key.keysym.sym == SDLK_k)
                    lineedit_delete_to_end();
                // ctrl-l -> 108 15 64
                if (e.key.keysym.sym == SDLK_l)
                    lineedit_clear_screen();
                // ctrl-n -> 110 17 64
                if (e.key.keysym.sym == SDLK_n)
                    lineedit_history_next();
                // ctrl-p ->  112 19 64
                if (e.key.keysym.sym == SDLK_p)
                    lineedit_history_prev();
                // ctrl-t ->  116 23 64
                if (e.key.keysym.sym == SDLK_t)
                    lineedit_swap_chars();
                // ctrl-u -> 117 24 64
                if (e.key.keysym.sym == SDLK_u)
                    lineedit_delete_line();
                // ctrl-w -> 119 26 64
                if (e.key.keysym.sym == SDLK_w)
                    lineedit_delete_word_prev();
            }
            // delete ->  127 76 0
            if (e.key.keysym.scancode == SDL_SCANCODE_DELETE)
                lineedit_delete();
            // down ->  1073741905 81 0
            if (e.key.keysym.scancode == SDL_SCANCODE_DOWN)
                lineedit_history_next();
            // end -> 1073741901 77 0
            if (e.key.keysym.scancode == SDL_SCANCODE_END)
                lineedit_move_end();
            // enter -> 13 40
            // home -> 1073741898 74 0
            if (e.key.keysym.scancode == SDL_SCANCODE_HOME)
                lineedit_move_home();
            // left -> 1073741904 80 0
            if (e.key.keysym.scancode == SDL_SCANCODE_LEFT)
                lineedit_move_left();
            // right ->  1073741903 79 0
            if (e.key.keysym.scancode == SDL_SCANCODE_RIGHT)
                lineedit_move_right();
            // up  -> 1073741906 82 0
            if (e.key.keysym.scancode == SDL_SCANCODE_UP)
                lineedit_history_prev();
            // KP_down -> 1073741914 90 0
            // KP_end -> 1073741913 89 0
            // KP_enter ->  1073741912 88
            // KP_home -> 1073741919 95 0
            // KP_left -> 1073741916 92 0
            // KP_right ->  1073741918 94 0
            // KP_up ->  1073741920 96 0
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
            /* printf("%u %u %d %d %d %d %d\n", e.key.timestamp, e.key.windowID, */
            /*        (int)e.key.state, (int)e.key.repeat, */
            /*        e.key.keysym.sym, e.key.keysym.scancode, e.key.keysym.mod); */
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
                        js_do_console_command(script);
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


static bool
on_idle (void)
{
    uint32_t cur_time;

    if (quitting_flag)
        return false;

    cur_time = SDL_GetTicks ();

    if (initialized_flag && !minimized_flag)
    {
        if (active_flag)
        {
            //audio_time_cur = cur_time;
            //audio_update();
            if (run_full_speed_flag || ((cur_time - before_update_time) > UPDATE_RATE))
            {

                js_do_idle (cur_time - before_update_time);

                update_count ++;
                before_update_time = cur_time;
                after_update_time = SDL_GetTicks();

                if (run_full_speed_flag || ((cur_time - before_draw_time) > REFRESH_RATE))
                {
                    paint ();
                    
                    js_do_after_draw_frame(after_draw_time - before_draw_time);
		  
                    draw_count ++;
                    before_draw_time = cur_time;
                    after_draw_time = SDL_GetTicks ();
		  
                    if (draw_count % 100 == 0)
                    {
                        measured_frame_rate 
                            = 100.0 / (after_draw_time - hundred_frames_draw_time);
                        hundred_frames_draw_time = after_draw_time;
                        SDL_LogDebug (SDL_LOG_CATEGORY_APPLICATION,
                                      "Frame Rate: %f\n",
                                      measured_frame_rate);
                    }
                }
            }

            if (!run_full_speed_flag)
                SDL_Delay(2);
        }
        else
        {
            //audio_pause ();
            // This really ought to wait for when a new window event
            // happens.
            SDL_Delay(2);
        }
    }

    return true;
}

static void paint (void)
{
    /* Have the engine render the backgrounds and objects to a bitmap */
    draw ();

    /* Have the bitmap pushed onto the screen. */
    eng_present ();
}
