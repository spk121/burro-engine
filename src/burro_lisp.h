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

/** Initialize a scheme module with Burro Engine's procedure.
 *  Return the module, or SCM_BOOL_F if there was an error.
 *  If err_string is not NULL, it will be set to a caller-freed
 *  error string.
 *
 * @param file 
 */
SCM burro_lisp_new (char **err_string);

/** Parse the contents of the file into the current module.
 */
gboolean burro_lisp_load (SCM module, GFile *file, char **err_string);

#endif
