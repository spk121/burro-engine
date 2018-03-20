/*  lisp.h

    Copyright (C) 2014, 2015, 2018   Michael L. Gran
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

/** @file lisp.h
 *  @brief set up custom scheme interpreter
 *
 *  The custom scheme interpreter is all callback based because this is a
 *  Gtk event loop program
 *
 *  The main module needs to to a few things.
 *  - it needs to provide on-idle and after-draw callback functions.
 *    Each procedure receives one argument, the current loop time.
 *  - it needs to register these callbacks with the engine using
 *    'loop-set-idle-callback' and 'loop-set-after-draw-callback'
*/

#ifndef BURRO_LISP_H
#define BURRO_LISP_H

/** Initialize the scheme interpreter.  Also, choose a module to be
 *  the main module.  It is expected that the main module will provide
 *  the on-idle and after-draw callback.
 *
 * @param main_module - a filename to a Guile module to be loaded.
 */
void init_lisp (const char *main_script);

void lisp_init_guile_procedures (void);

SCM G_restart();

#endif
