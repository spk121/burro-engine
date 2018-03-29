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
*/

#ifndef _BURRO_LISP_H
#define _BURRO_LISP_H

#include <gtk/gtk.h>
#include <libguile.h>

SCM burro_lisp_new ();

/** Parse the contents of the file into the current module.
 */
SCM burro_make_sandbox (GFile *file, char **err_string);

#endif
