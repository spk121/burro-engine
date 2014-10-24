/* Interpreter.hpp -- the SpiderMonkey binding

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

#ifndef BURRO_INTERPRETER_H
#define BURRO_INTERPRETER_H
#include <string>
#include "jsapi.hpp"

using namespace std;

enum Interpreter_event {
    EVENT_UP,
    EVENT_DOWN,
    EVENT_LEFT,
    EVENT_RIGHT
};

class Interpreter {
private:
    JSClass global_class;
    JSRuntime *rt;
    JSContext *cx;
    JSObject *global;

private:
    void Do_uint32_func (const string& name, uint32_t val);
    
public:
    Interpreter();
    int Initialize();
    int Finalize();
    void Parse (const string& resource_name);
    void Do_console_command (char *str);
    void Do_idle(uint32_t delta_t);
    void Do_after_draw_frame(uint32_t delta_t);
    void Do_keypress (Interpreter_event event, bool begin, uint32_t ticks);
};

extern Interpreter interpreter;

#endif
