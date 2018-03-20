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
#include "../x/xglib.h"
#include "rand.h"

static GRand *r;

void rand_init (void)
{
  r = xg_rand_new ();
}

void rand_init_with_seed (guint32 seed)
{
  r = xg_rand_new_with_seed (seed);
}

gint32 rand_int_range (gint32 begin, gint32 end)
{
  g_return_val_if_fail (begin < end, rand_int_range (end, begin));
  g_return_val_if_fail (begin != end, begin);

  return xg_rand_int_range (r, begin, end);
}


