/*  lisp.c

    Copyright (C) 2018   Michael L. Gran
    This file is part of Burro Engine

    Burro Engine is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Burro Engine is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Burro Engine.  If not, see <http://www.gnu.org/licenses/>.
*/
#include "../x.h"
#include "../paths.h"
#include <glib.h>
#include "audio_model.h"
#include "backdrop.h"
#include "bg.h"
#include "console.h"
#include "eng.h"
#include "guile.h"
#include "lisp.h"
#include "loop.h"
#include "obj.h"
#include "pixbuf.h"
#include "repl.h"
#include "textbox.h"
#include <libguile.h>

// One route to defining C functions into a module is
// scm_c_resolve_module to open and create if necessary a module
// SCM val = scm_c_make_gsubr(subr args and such)  to make it
// scm_c_module_define (module, c_name, val)
// but are they public?

// There is also scm_c_define_module where I could 

void
_burroscript_init (void *unused)
{
    am_init_guile_procedures ();
    backdrop_init_guile_procedures ();
    bg_init_guile_procedures();
    console_init_guile_procedures();
    eng_init_guile_procedures();
    guile_init_guile_procedures();
    lisp_init_guile_procedures();
    loop_init_guile_procedures();
    matrix_init_guile_procedures();
    vram_init_guile_procedures();
    pixbuf_init_guile_procedures();
    obj_init_guile_procedures();
    textbox_init_guile_procedures();

    char *libstr = xg_resources_get_string("/com/lonelycactus/burroengine/library.scm");
    xscm_c_eval_string_or_warn (libstr);
    free(libstr);
}

void
init_lisp (const char *main_script)
{
    /* Redirect output/error to console */
    // FIXME: do this

    // We start off with what is in the guile-user module and add
    // to it from there.
    SCM burro_user_module = scm_c_define_module ("guile-user", NULL, NULL);
    scm_set_current_module (burro_user_module);

    scm_c_use_module ("ice-9 readline");
    scm_c_use_module ("ice-9 eval-string");
    scm_c_use_module ("srfi srfi-1");
    scm_c_use_module ("rnrs bytevectors");
    scm_c_use_module ("system repl repl");
    scm_c_use_module ("system repl server");
    scm_c_use_module ("system repl coop-server");
    scm_c_use_module ("system vm trap-state");
    
    // Define the Burro module that contains our internal functions,
    // and then load it.
    scm_c_define_module ("burro", _burroscript_init, NULL);
    scm_c_use_module ("burro");

    if (main_script)
        xscm_c_primitive_load(main_script);
    else
    {
        console_write_ecma48_string ("Burro Engine\r\n\r\n");
        console_write_ecma48_string ("No game file was provided.\r\n");
        console_write_ecma48_string ("Starting in debug mode.\r\n");
        console_write_ecma48_string ("You may enter commands here.\r\n");
        console_write_ecma48_string ("Also, you may enter commands on TCP port 37147\r\n");
        console_write_ecma48_string ("Enter \"(console-hide)\" here to hide this console,\r\n");
        console_write_ecma48_string ("and press the tilde key to bring it back again later.\r\n");
        repl_init ();
        console_enable_repl ();
    }
}

////////////////////////////////////////////////////////////////
// Here are some random functions for use with the console


#if 0
SCM_DEFINE (G_restart, "burro-restart", 0, 0, 0, (void), "\
Reload and re-eval the main script.\n")
{
    // FIXME: this is probably where we decide between scheme and wisp
    char *cmd = g_strdup_printf("(load-from-path \"%s\")", lisp_main_script);
    SCM ret = xscm_c_eval_string_or_warn(cmd);
    g_free (cmd);
    return ret;
}
#endif

SCM_DEFINE (G_list_data_directory_contents, "burro-dir", 0, 1, 0, (SCM regex), "\
List the contents of the data directory.  If a parameter is provided, \n\
it is used as a regular expression for matching.\n")
{
    GError *error = NULL;
    
    const char *assets_dir = g_getenv("BURRO_DATA_DIR");
#ifdef BURRO_DATA_DIR
    if (assets_dir == NULL)
        assets_dir = BURRO_DATA_DIR;
#endif
    
    GDir *dir = g_dir_open (assets_dir, 0, &error);
    if (dir == NULL)
    {
        g_critical ("can't read BURRO data dir: %s", error->message);
        g_error_free (error);
        return SCM_UNSPECIFIED;
    }

#if 0    
    console_write_utf8_string("in ");
    console_write_utf8_string(path);
    console_move_down(1);
    console_move_to_column(0);
    console_write_utf8_string("-----------------------------");
    console_move_down(1);
    console_move_to_column(0);

    const char *fname;
    while ((fname = g_dir_read_name (dir)) != NULL)
    {
        console_write_utf8_string (fname);
        console_move_down(1);
        console_move_to_column(0);
    }
    
#else
    scm_display(scm_from_utf8_string("in "), scm_current_output_port());
    scm_display(scm_from_utf8_string(assets_dir), scm_current_output_port());
    scm_newline(scm_current_output_port());
    scm_display(scm_from_utf8_string("-----------------------------"), scm_current_output_port());
    scm_newline(scm_current_output_port());

    const char *fname;
    while ((fname = g_dir_read_name (dir)) != NULL)
    {
        scm_display(scm_from_utf8_string(fname), scm_current_output_port());
        scm_newline(scm_current_output_port());
    }
#endif
    g_dir_close (dir);
    return SCM_UNSPECIFIED;
}


#if 0
SCM_DEFINE (G_console, "console", 0, 0, 0, (void), "\
Suspend the editor and bring up the REPL for this buffer's module.")
{
    SCM var, func;

    /* Suspend the CRT screen handling.  */
    // interactive = false;
    // set_error_output_to_console ();

    console_reset ();
#define W console_write_latin1_string  
    W ("+-------------------------------------------------------+\n");
    W ("| This is the debug console for this buffer.            |\n");
    W ("| \"(quit)\" to quit back to the editor                   |\n");
    W ("| \"(burro-procedures)\" to see burro functions           |\n");
    W ("| \"(key-map)\" to see game's keymap                      |\n");
    W ("+-------------------------------------------------------+\n");
#undef W
  
    /* Switch to the current buffer's module.  */
    scm_c_use_module ("system repl repl");
    var = scm_c_lookup ("start-repl");
    func = scm_variable_ref (var);
    scm_call_0 (func);

    printf ("Press <ENTER> key to re-enter zile...\n");
    fflush (stdout);
    // set_error_output_to_minibuffer ();
    // scm_read_char (scm_current_input_port ());
    // interactive = true;
    return SCM_BOOL_T;
}

#endif

void
lisp_init_guile_procedures (void)
{
#include "lisp.x"
    scm_c_export("burro-dir",
                 NULL);
}

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
