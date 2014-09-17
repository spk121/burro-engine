/* Background.hpp -- the background layers

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

#ifndef BURRO_BACKGROUND_H
#define BURRO_BACKGROUND_H

#include "ResCache.hpp"
#include "Tmx.h"
#include "const.h"
#include "xcairo.hpp"
#include <array>

using namespace std;
using namespace Tmx;

struct Background_Layer {
    /* BG is display when true */
    bool enable;
    
    /** the "user" or screen location of the rotation center of the
     * background */
    double scroll_x, scroll_y;
    
    /** the "device" location of the rotation center of the
     * background*/
    double rotation_center_x, rotation_center_y;
    
    /** the expansion factor of the background: 1.0 = 1 pixel per
     * pixel */
    double expansion;
    
    /** the rotation angle of the background about its rotation
     * center, in radians */
    double rotation;
    
    /** The image data of the layer as a Cairo surface. */
    cairo_surface_t* surface;
};

struct Background {
	bool colorswap;
	double brightness;
    array<Background_Layer, MAIN_BACKGROUNDS_COUNT> layers;

    void Reset();
    void Update_layer_from_cairo_surface (cairo_surface_t *layer, int z_level);
    void Update_layers_from_tmx_map(Map& map, ResCache& resource_cache);
    void Draw_zlevel_to_context(cairo_t *screen, int z_level);
    
};

extern Background bg;

#endif
