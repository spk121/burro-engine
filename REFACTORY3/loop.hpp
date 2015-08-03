/* loop.hpp -- the main loop

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

#ifndef BURRO_LOOP_H
#define BURRO_LOOP_H

class Loop {
private:
	bool active_flag;
	bool console_flag;			// True when text input goes to the console
	bool autocomplete_flag;
	bool initialized_flag;
	bool minimized_flag;
	bool quitting_flag;
	bool run_full_speed_flag;
	int update_count;
	int draw_count;

	uint32_t before_update_time;
	uint32_t after_update_time;
	uint32_t before_draw_time;
	uint32_t after_draw_time;
	uint32_t hundred_frames_draw_time;
	double measured_frame_rate;

public:
	// Character storage for the line editor
	// FIXME: move this
	wchar_t buf[200];

public:
	Loop();
	void go();
	void set_console_mode (bool flag);
    bool get_console_mode () {return console_flag;}

private:
	void do_console_event (SDL_Event e);
	void do_keypress_event (SDL_Event e);
	bool on_idle();
};

extern Loop loop;
extern JSFunctionSpec game_loop_functions[2];

#endif
