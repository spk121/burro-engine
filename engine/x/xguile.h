/*  xguile.h

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
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef BURRO_XGUILE_H
#define BURRO_XGUILE_H

#include <stdbool.h>
#include <libguile.h>

SCM                 xscm_c_eval_string                  (const char *string);
SCM                 xscm_c_eval_string_or_warn          (const char *string);
SCM                 xscm_c_primitive_load               (const char *filename);
SCM                 xscm_from_latin1_symbol             (const char *name);
void                xscm_init_guile                     (void);
SCM                 xscm_lookup                         (SCM name);
bool                xscm_is_variable                    (SCM x);
bool                xscm_is_symbol                      (SCM x);
bool                xscm_is_procedure                   (SCM x);

#endif
