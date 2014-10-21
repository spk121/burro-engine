/* js.cxx -- the SpiderMonkey binding

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

#include <SDL.h>
#include <vector>

#include "jsapi.hpp"

#include "Interpreter.hpp"
#include "ResourceFile.hpp"
#include "backdrop.hpp"
#include "bg.hpp"
#include "console.hpp"
#include "eng.hpp"
#include "obj.hpp"
#include "loop.hpp"
#include "xsdl.hpp"
#include "ecma48_test.hpp"
#include "tmx.hpp"
#include "Background.hpp"

#ifdef JS_SCRIPT_IS_PACKED
extern char _binary_script_js_start[];
extern char _binary_script_js_end[];
#endif

static void
js_console_error_reporter(JSContext *cx, const char *message, JSErrorReport *report);

Interpreter interpreter{};

Interpreter::Interpreter()
    : global_class{ "global",
        JSCLASS_NEW_RESOLVE | JSCLASS_GLOBAL_FLAGS,
        JS_PropertyStub,
        JS_DeletePropertyStub,
        JS_PropertyStub,
        JS_StrictPropertyStub,
        JS_EnumerateStub,
        JS_ResolveStub,
        JS_ConvertStub,
        nullptr,
        JSCLASS_NO_OPTIONAL_MEMBERS},
    rt {nullptr},
    cx {nullptr},
    global {nullptr}
{
}

int Interpreter::Initialize()
{
    // JS_Init();

    rt = JS_NewRuntime(8L * 1024 * 1024, JS_NO_HELPER_THREADS);
    if (!rt)
        return 1;

    cx = JS_NewContext(rt, 8192);
    if (!cx)
        return 1;

    /* Enter a request before running anything in the context */
    JSAutoRequest ar(cx);

    /* Create the global object in a new compartment. */
    global = JS_NewGlobalObject(cx, &global_class, nullptr);
    if (!global) {
        fprintf(stderr, "can't create global object for javascript instance");
        abort();
    }

    /* Set the context's global */
    JSAutoCompartment ac(cx, global);
    JS_SetGlobalObject(cx, global);

    /* Populate the global object with the standard globals, like Object and Array. */
    if (!JS_InitStandardClasses(cx, global))
        return 1;

    vector<const JSFunctionSpec *> functions {
        backdrop_functions,
            // bg_functions,
            console_functions,
            eng_functions,
            obj_functions,
            game_loop_functions,
            ecma48_test_functions,
            tmx_functions,
            background_functions
            };
    
    all_of (functions.begin(),
            functions.end(),
            [=](const JSFunctionSpec *list) {
                return (JS_DefineFunctions(cx, global, (list)) == JS_TRUE);
            });
    
    JS_SetErrorReporter(cx, js_console_error_reporter);
    return 0;
}

void Interpreter::Parse (const string& resource_name)
{
    // Load in Javascript from the data path
    vector<char> v = resource_file.Get_data (resource_name);
    string s(v.begin(), v.end());

    JSAutoCompartment ac(cx, global);

    JS::Value rval;
    JS_EvaluateScript(cx, global, s.c_str(), strlen(s.c_str()), resource_name.c_str(), 0, &rval);
}

int Interpreter::Finalize(void)
{
    JS_DestroyContext(cx);
    JS_DestroyRuntime(rt);
    JS_ShutDown();
    return 0;
}

void Interpreter::Do_uint32_func (const string& name, uint32_t val)
{
    JS::Value argv[1], rval;
    argv[0].setNumber(val);
    JSAutoCompartment ac(cx, global);
    JS_CallFunctionName(cx, global, name.c_str(), 1, argv, &rval);
}

void Interpreter::Do_idle(uint32_t delta_t)
{
    Do_uint32_func("DoIdle", delta_t);
}

void Interpreter::Do_after_draw_frame(uint32_t delta_t)
{
    Do_uint32_func("DoAfterDrawFrame", delta_t);
}

void Interpreter::Do_console_command (char *str)
{
    JSAutoCompartment ac(cx, global);

    JS::Value rval;

    if (JS_EvaluateScript(cx, global, str, strlen(str), "console", 0, &rval) == JS_TRUE)
    {
        console_move_to_column(0);
        console_erase_to_end_of_line();

        if (!rval.isUndefined())
        {
            JSString *rval_as_jsstring = JS_ValueToString(cx, rval);
            if (rval_as_jsstring)
            {
                char *rval_as_str = JS_EncodeString(cx, rval_as_jsstring);
                if (rval_as_str)
                    console_write_utf8_string(rval_as_str);
                else
                    // String can't be represented in the current encoding
                    console_write_utf8_string("(unencodeable)");
            }
            else
                // rval can be represented as a string
                console_write_utf8_string("(unexpressable)");
        }
        else
            // Function doesn't return anything
            console_write_utf8_string("OK");

        console_move_down(1);
        console_move_to_column(0);
    }
    else
    {
        // compilation failure
        JS_ReportPendingException(cx);
    }
}

static void
js_console_error_reporter(JSContext *cx, const char *message, JSErrorReport *report)
{
    console_move_to_column(0);
    console_erase_to_end_of_line();
    console_write_utf8_string(message);
    console_move_down(1);
    console_move_to_column(0);
}

