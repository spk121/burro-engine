/*  rand.c

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
    along with Burro Engin.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef BURRO_RAND_H
#define BURRO_RAND_H

void rand_init (void);
void rand_init_with_seed (guint32 seed);
gint32 rand_int_range (gint32 begin, gint32 end);
void rand_fini (void);
#endif
