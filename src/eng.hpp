/* eng.hpp -- Top-level graphics engine

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

#ifndef BURRO_ENG_H
#define BURRO_ENG_H

#include "jsapi.hpp"

void eng_initialize ();

void eng_present();
void eng_finalize();
void eng_initialize();
void eng_set_brightness(double b);
double eng_get_brightness();
void eng_uncolorswap();
void eng_colorswap();
bool eng_is_colorswap();
void eng_unblank();
void eng_blank();
bool eng_is_blank();

extern JSFunctionSpec eng_functions[9];
#endif
