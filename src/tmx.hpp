#ifndef BURRO_TMX_H
#define BURRO_TMX_H

#include "xcairo.hpp"
#include "Tmx.h"
#include "ResCache.hpp"

using namespace std;
using namespace Tmx;

int tmx_load(string mapname);
int tmx_debug();
cairo_surface_t* tmx_render_layer_to_cairo_surface(Map &map, int z_level, ResCache& resource_cache);
cairo_surface_t* tmx_render_image_layer_to_cairo_surface(Map& map1, ImageLayer* layer, ResCache& resource_cache);
cairo_surface_t* tmx_render_map_layer_to_cairo_surface(Map& map1, Layer* layer, ResCache& resource_cache);

#endif
