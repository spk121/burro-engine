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

#include "jsapi.hpp"

#include "Interpreter.hpp"
#include "backdrop.hpp"
#include "bg.hpp"
#include "console.hpp"
#include "eng.hpp"
#include "obj.hpp"
#include "loop.hpp"
#include "xsdl.hpp"

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

int Interpreter::initialize(bool unpacked_flag)
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
	
    if (!JS_DefineFunctions(cx, global, backdrop_functions))
        return false;
    if (!JS_DefineFunctions(cx, global, bg_functions))
        return false;
    if (!JS_DefineFunctions(cx, global, console_functions))
        return false;
    if (!JS_DefineFunctions(cx, global, eng_functions))
        return false;
    if (!JS_DefineFunctions(cx, global, obj_functions))
        return false;
    if (!JS_DefineFunctions(cx, global, game_loop_functions))
        return false;
	
	
    JS_SetErrorReporter(cx, js_console_error_reporter);
	
    if (unpacked_flag) {
        // Load in Javascript from the data path
        SDL_RWops *fp = xSDL_RWFromDataFile("script.js", "r");
        int64_t len = xSDL_RWseek(fp, 0, RW_SEEK_END);
        xSDL_RWseek(fp, 0, RW_SEEK_SET);
        char buf[len];          // FIXME - VLA
        SDL_RWread(fp, buf, sizeof(buf), 1);
        SDL_RWclose(fp);
        JS::Value rval;
        JSBool ok = JS_EvaluateScript(cx, global, buf, len, "script.js", 0, &rval);
        SDL_assert(ok);
    }
    else {
        // Load in Javascript from a data partition embedded in this executable
#ifdef JS_SCRIPT_IS_PACKED
        JS::Value rval;
        JSBool ok;
        ok = JS_EvaluateScript(cx, global, _binary_script_js_start,
                               _binary_script_js_end - _binary_script_js_start,
                               "(internal)", 0, &rval);
        if (ok && !rval.isUndefined()) {

            JSString *rval_as_jsstring = JS_ValueToString(cx, rval);
            if (!rval_as_jsstring)
                goto cont;
            char *rval_as_str = JS_EncodeString(cx, rval_as_jsstring);
            if (!rval_as_str)
                goto cont;
            printf("%s\n", rval_as_str);
        }
#else
        printf("Error: no packed js script\n");
        exit(1);
#endif
    }
#ifdef JS_SCRIPT_IS_PACKED
 cont:
#endif
    
    return 0;
}

int Interpreter::finalize(void)
{
    JS_DestroyContext(cx);
    JS_DestroyRuntime(rt);
    JS_ShutDown();
    return 0;
}

void Interpreter::do_idle(uint32_t delta_t)
{
    JS::Value argv[1], rval;
    argv[0].setNumber(delta_t);
    JSAutoCompartment ac(cx, global);
    JS_CallFunctionName(cx, global, "DoIdle", 1, argv, &rval);
}

void Interpreter::do_after_draw_frame(uint32_t delta_t)
{
    JS::Value argv[1], rval;
    argv[0].setNumber(delta_t);
    JSAutoCompartment ac(cx, global);
    JS_CallFunctionName(cx, global, "DoAfterDrawFrame", 1, argv, &rval);
}

void Interpreter::do_console_command (char *str)
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

