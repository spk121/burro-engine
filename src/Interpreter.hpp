/* js.hpp -- the SpiderMonkey binding

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

#ifndef BURRO_JS_H
#define BURRO_JS_H

#include "jsapi.hpp"

class Interpreter {
private:
	JSClass global_class;
	JSRuntime *rt;
	JSContext *cx;
	JSObject *global;

public:
	Interpreter();
	int initialize(bool unpacked_flag);
	int finalize();
	void do_console_command (char *str);
	void do_idle(uint32_t delta_t);
	void do_after_draw_frame(uint32_t delta_t);
};

extern Interpreter interpreter;

#endif
