//-----------------------------------------------------------------------------
// Copyright (c) 2010-2014, Tamir Atias
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//  * Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
//  * Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL TAMIR ATIAS BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//-----------------------------------------------------------------------------
#include "const.h"
#include "Tmx.h"
#include "xsdl.hpp"
#include "tmx.hpp"
// #include "bg2.hpp"
#include "xiso9660.hpp"
#include "xcairo.hpp"
#include "ResCache.hpp"
#include "tmx.hpp"
#include <cstdio>
#include <cstdlib>
#include <algorithm>

using namespace std;
using namespace Tmx;

void tmx_draw_layer(cairo_t* screen, Map& map, Layer* layer);
cairo_surface_t* tmx_render_layer_to_cairo_surface(Map &map, int z_level, ResCache& resource_cache)
{
    for (auto layer : map.GetLayers()) {
        if (z_level == layer->GetZOrder() && layer->IsVisible()) {
            cairo_surface_t* layer_surface = tmx_render_map_layer_to_cairo_surface(map, layer, resource_cache);
            return layer_surface;
        }
    }
    for (auto layer: map.GetImageLayers()) {
        if (z_level == layer->GetZOrder() && layer->IsVisible()) {
            cairo_surface_t *layer_surface = tmx_render_image_layer_to_cairo_surface(map, layer, resource_cache);
            return layer_surface;
        }
    }
    return NULL;
}

cairo_surface_t* tmx_render_image_layer_to_cairo_surface(Map& map1, ImageLayer* layer, ResCache& resource_cache)
{
    return nullptr;
}


cairo_surface_t* tmx_render_map_layer_to_cairo_surface(Map& map1, Layer* layer, ResCache& resource_cache)
{
    int target_width_in_pixels = map1.GetTileWidth() * map1.GetWidth();
    int target_height_in_pixels = map1.GetTileHeight() * map1.GetHeight();
    
    cairo_surface_t* target_surface = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                                                   target_width_in_pixels,
                                                                   target_height_in_pixels);
    // cairo_t*  target_context = xcairo_create (target_surface);
    
    uint32_t* target_argb32_data = xcairo_image_surface_get_argb32_data (target_surface);
    int target_stride = xcairo_image_surface_get_argb32_stride (target_surface);
    xcairo_surface_flush (target_surface);

    for (int target_tile_row_index; target_tile_row_index < map1.GetHeight(); target_tile_row_index ++) {
        for (int target_tile_column_index; target_tile_column_index < map1.GetWidth(); target_tile_column_index ++) {
            const Tileset *tilesheet_for_current_tile = map1.FindTileset(layer->GetTileId(target_tile_column_index, target_tile_row_index));
            const Image *tmx_image_for_current_tile = tilesheet_for_current_tile->GetImage();
            cairo_surface_t* tilesheet_surface = (cairo_surface_t*) resource_cache.Get(quick_hash(tmx_image_for_current_tile->GetSource()));
            uint32_t* tilesheet_argb32_data = xcairo_image_surface_get_argb32_data (tilesheet_surface);
            
            int map_index  = layer->GetTileId(target_tile_column_index, target_tile_row_index) - tilesheet_for_current_tile->GetFirstGid();
            int image_width = tmx_image_for_current_tile->GetWidth();
            // int image_height = tmx_image_for_current_tile->GetHeight();
            int tilesheet_margin = tilesheet_for_current_tile->GetMargin();
            int tilesheet_spacing = tilesheet_for_current_tile->GetSpacing();
            int tile_width = tilesheet_for_current_tile->GetTileWidth();
            int tile_height = tilesheet_for_current_tile->GetTileHeight();
            int tilesheet_width_in_tiles = (image_width - 2*tilesheet_margin) / (tile_width + tilesheet_spacing);
            // int tilesheet_height_in_tiles = (image_height - 2*tilesheet_margin) / (tile_height + tilesheet_spacing);
            int tilesheet_tile_row_index = map_index / tilesheet_width_in_tiles;
            int tilesheet_tile_column_index = map_index % tilesheet_width_in_tiles;
            int tilesheet_pixel_column_index_start = tilesheet_margin + tilesheet_tile_column_index * (tile_width + tilesheet_spacing);
            int tilesheet_pixel_row_index_start = tilesheet_margin + tilesheet_tile_row_index * (tile_height + tilesheet_spacing);
            int tilesheet_stride = xcairo_image_surface_get_argb32_stride (tilesheet_surface);
            for (int tile_pixel_row_index = 0; tile_pixel_row_index < tile_height; tile_pixel_row_index ++) {
                for (int tile_pixel_column_index = 0; tile_pixel_column_index < tile_width; tile_pixel_column_index ++) {
                    uint32_t c = tilesheet_argb32_data[(tilesheet_pixel_row_index_start + tile_pixel_row_index) * tilesheet_stride
                                                       + tilesheet_pixel_column_index_start + tile_pixel_column_index];
                    // FIXME: handle transparent color
                    // FIXME: handle layer global transparency
                    target_argb32_data[(target_tile_row_index * tile_height + tile_pixel_row_index) * target_stride
                         + (target_tile_column_index * tile_width + tile_pixel_column_index)] = c;
                }
            }
            
        }
    }
    xcairo_surface_mark_dirty(target_surface);
    return target_surface;
}


#if 0
void tmx_draw_layer(cairo_t*, Map& map, Layer* layer, ResCache& resource_cache)
{
    cairo_surface_t *surf = tmx_render_map_layer_to_cairo_surface(map, layer, resource_cache);
    if (!layer->IsVisible())
        return;
    for (int y = 0; y < layer->GetHeight(); ++y) {
        for (int x = 0; x < layer->GetWidth(); ++x) {
            // Get the tile's id.
            printf("%03d", layer->GetTileId(x, y));
            
            //  Find a tileset for that id.
            const Tileset *tileset = map.FindTileset(layer->GetTileId(x, y));
            const Image *image = tileset->GetImage();
            int image_width = image->GetWidth();
            int image_height = image->GetHeight();
            int tileset_margin = tileset->GetMargin();
            int tileset_spacing = tileset->GetSpacing();
            int tile_width = tileset->GetTileWidth();
            int tile_height = tileset->GetTileHeight();
            int image_width_in_tiles = (image_width - 2*tileset_margin) / (tile_width + tileset_spacing);
            int image_height_in_tiles = (image_height - 2*tileset_margin) / (tile_height + tileset_spacing);

            int tile_in_tileset = layer->GetTileId(x, y) - tileset->GetFirstGid();
            int tileset_column = (tile_in_tileset % image_width_in_tiles);
            int tileset_row = (tile_in_tileset / image_height_in_tiles);
            int source_startx = tileset_margin + tileset_column * (tile_width + tileset_spacing);
            int source_starty = tileset_margin + tileset_row * (tile_height + tileset_spacing);

            int target_startx = x * tile_width;
            int target_starty = y * tile_height;
            
            if (layer->IsTileFlippedHorizontally(x, y)){
                printf("h");
            }else{
                printf(" ");
            }
            if (layer->IsTileFlippedVertically(x, y)){
                printf("v");
            }else{
                printf(" ");
            }
            if (layer->IsTileFlippedDiagonally(x, y)){
                printf("d ");
            } else {
                printf("  ");
            }
        }
    }
}
#endif

#if 0
int tmx_prevalidate_map(Map& map, string& path)
{

	if (map.GetWidth() > MAP_WIDTH) {
		SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
					 "Map width (%d) is greater than the max allowed (%d): %s",
					 map.GetWidth(),
					 MAP_WIDTH,
					 path.c_str());
	}
	if (map.GetHeight() > MAP_HEIGHT) {
		SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
					 "Map height (%d) is greater than the max allowed (%d): %s",
					 map.GetHeight(),
					 MAP_HEIGHT,
					 path.c_str());
	}
	if (map.GetTileWidth() != TILE_WIDTH) {
		SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
					 "Tile width (%d) is not the allowed value (%d): %s",
					 map.GetTileWidth(),
					 TILE_WIDTH,
					 path.c_str());
	}
	if (map.GetTileWidth() != TILE_HEIGHT) {
		SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
					 "Tile height (%d) is not the allowed value (%d): %s",
					 map.GetTileHeight(),
					 TILE_HEIGHT,
					 path.c_str());
	}

	if (map.GetNumTilesets() != 1) {
		SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
					 "Number of tilesets (%d) is not 1: %s",
					 map.GetNumTilesets(),
					 path.c_str());
	}
	if (map.GetNumLayers() <= 0 || map.GetNumLayers() > MAIN_BACKGROUNDS_COUNT) {
		SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
					 "The number of layers (%d) is not 1 to %d: %s",
					 map.GetNumLayers(),
					 MAIN_BACKGROUNDS_COUNT,
					 path.c_str());
	}
	return true;
 }



int tmx_load(string mapname)
{
	Map map {};
	string path {xsdl_get_data_path()};

	path.append(mapname);
	map.ParseFile(mapname);
	if (map.HasError()) {
		SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
					 "Can't open map file: %s, %d, %s",
					 path.c_str(),
					 map.GetErrorCode(),
					 map.GetErrorText().c_str());
	}

	if (tmx_prevalidate_map(map, path) == false)
		return 1;

	// Properties of the map
	auto map_properties = map.GetProperties().GetList();
	bg.colorswap = stoi(map_properties["colorswap"]);
	bg.brightness = stod(map_properties["brightness"]);

	int map_width = map.GetWidth(); // In tiles
	int map_height = map.GetHeight();

	
	// Load the map layers. A vector of pointers to layers.
	auto layers = map.GetLayers();
	bg.layers.clear();
	for_each(layers.begin(), layers.end(),
			 [](Layer* layer) { bg.add_tmx_layer(layer); });

	bg.tilesets.clear();
	auto tilesets = map.GetTilesets();
	for_each(tilesets.begin(), tilesets.end(),
			 [](Tileset* tileset) { bg.add_tmx_tileset(tileset); });

	
	return 0;
}
#endif

int tmx_debug() {
    string data_path{xsdl_get_data_path()};
    data_path.append("burro.iso");

    ISO9660::IFS handle = xiso9660_open(data_path);
    xiso9660_initialize_hash(handle);
    vector<char> map_data = xiso9660_get_data(handle, "map1.tmx");

    Map map {};
    map.ParseText(string(map_data.data(), map_data.size()));

	if (map.HasError()) {
		printf("error code: %d\n", map.GetErrorCode());
		printf("error text: %s\n", map.GetErrorText().c_str());

		return map.GetErrorCode();
	}

    // Create a resource cache
    ResCache rc {10, "burro.iso"};
    
	// Iterate through the tilesets.
	for (int i = 0; i < map.GetNumTilesets(); ++i) {
		printf("                                    \n");
		printf("====================================\n");
		printf("Tileset : %02d\n", i);
		printf("====================================\n");

		// Get a tileset.
		const Tmx::Tileset *tileset = map.GetTileset(i);

		// Print tileset information.
		printf("Name: %s\n", tileset->GetName().c_str());
		printf("Margin: %d\n", tileset->GetMargin());
		printf("Spacing: %d\n", tileset->GetSpacing());
		printf("Image Width: %d\n", tileset->GetImage()->GetWidth());
		printf("Image Height: %d\n", tileset->GetImage()->GetHeight());
		printf("Image Source: %s\n", tileset->GetImage()->GetSource().c_str());
		printf("Transparent Color (hex): %s\n", tileset->GetImage()->GetTransparentColor().c_str());

        // Load the tileset into the resource cache
        unsigned int hash = quick_hash(tileset->GetImage()->GetSource());
        printf("Tileset Hash ID: %d\n", hash);
        void *tile_data = rc.Get(hash);
        if (tile_data != NULL)
            printf("Tile Data: %p\n", tile_data);
        else
            printf("Tile Data: NOT FOUND\n");
        
        #if 0
		if (tileset->GetTiles().size() > 0) {
			// Get a tile from the tileset.
			const Tmx::Tile *tile = *(tileset->GetTiles().begin());

			// Print the properties of a tile.
			std::map< std::string, std::string > list = tile->GetProperties().GetList();
			std::map< std::string, std::string >::iterator iter;
			for (iter = list.begin(); iter != list.end(); ++iter) {
				printf("%s = %s\n", iter->first.c_str(), iter->second.c_str());
			}
		}
        #endif
	}

	// Iterate through the layers.
	for (int i = 0; i < map.GetNumLayers(); ++i) {
		printf("                                    \n");
		printf("====================================\n");
		printf("Layer : %02d/%s \n", i, map.GetLayer(i)->GetName().c_str());
		printf("====================================\n");
	
		// Get a layer.
		const Tmx::Layer *layer = map.GetLayer(i);

		for (int y = 0; y < layer->GetHeight(); ++y) {
			for (int x = 0; x < layer->GetWidth(); ++x) {
				// Get the tile's id.
				printf("%03d", layer->GetTileId(x, y));

				// Find a tileset for that id.
				//const Tmx::Tileset *tileset = map.FindTileset(layer->GetTileId(x, y));
				if (layer->IsTileFlippedHorizontally(x, y)){
					printf("h");
				}else{
					printf(" ");
				}
				if (layer->IsTileFlippedVertically(x, y)){
					printf("v");
				}else{
					printf(" ");
				}
				if (layer->IsTileFlippedDiagonally(x, y)){
					printf("d ");
				} else {
					printf("  ");
				}
			}

			printf("\n");
		}
	}

	printf("\n\n");

	// Iterate through all of the object groups.
	for (int i = 0; i < map.GetNumObjectGroups(); ++i) {
		printf("                                    \n");
		printf("====================================\n");
		printf("Object group : %02d\n", i);
		printf("====================================\n");

		// Get an object group.
		const Tmx::ObjectGroup *objectGroup = map.GetObjectGroup(i);

		// Iterate through all objects in the object group.
		for (int j = 0; j < objectGroup->GetNumObjects(); ++j) {
			// Get an object.
			const Tmx::Object *object = objectGroup->GetObject(j);

			// Print information about the object.
			printf("Object Name: %s\n", object->GetName().c_str());
			printf("Object Position: (%03d, %03d)\n", object->GetX(), object->GetY());
			printf("Object Size: (%03d, %03d)\n", object->GetWidth(), object->GetHeight());

			// Print Polygon points.
			const Tmx::Polygon *polygon = object->GetPolygon();
			if (polygon != 0) {
				for (int i = 0; i < polygon->GetNumPoints(); i++) {
					const Tmx::Point &point = polygon->GetPoint(i);
					printf("Object Polygon: Point %d: (%d, %d)\n", i, point.x, point.y);
				}
			}

			// Print Polyline points.
			const Tmx::Polyline *polyline = object->GetPolyline();
			if (polyline != 0) {
				for (int i = 0; i < polyline->GetNumPoints(); i++) {
					const Tmx::Point &point = polyline->GetPoint(i);
					printf("Object Polyline: Point %d: (%d, %d)\n", i, point.x, point.y);
				}
			}
		}
	}

	return 0;
}
