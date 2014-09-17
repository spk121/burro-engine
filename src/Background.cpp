#include "Background.hpp"
#include "xcairo.hpp"
#include "ResCache.hpp"
#include "Tmx.h"
#include "tmx.hpp"
#include "xsdl.hpp"


Background bg {};

static cairo_matrix_t compute_transform (double rotation_center_screen_x,
                                         double rotation_center_screen_y,
                                         int rotation_center_bitmap_row,
                                         int rotation_center_bitmap_column,
                                         double rotation_angle,
                                         double expansion_factor)
{
    double xx, xy, yx, yy, x0, y0;
    double sn, cs;
    cairo_matrix_t matrix;
    if (expansion_factor == 0.0)
        expansion_factor = 1.0;
    sn = sin (rotation_angle);
    cs = cos (rotation_angle);
    xx = expansion_factor * cs;
    xy = expansion_factor * sn;
    yx = -xy;
    yy = xx;
    x0 = (rotation_center_screen_x
          - (xx * (double)rotation_center_bitmap_column
             + xy * (double)rotation_center_bitmap_row));
    y0 = (rotation_center_screen_y
          - (yx * (double)rotation_center_bitmap_column
             + yy * (double)rotation_center_bitmap_row));
    matrix.xx = xx;
    matrix.xy = xy;
    matrix.yx = yx;
    matrix.yy = yy;
    matrix.x0 = x0;
    matrix.y0 = y0;
    return matrix;
}

static void paint_transformed_image (cairo_t *context,
                                     cairo_matrix_t *matrix,
                                     cairo_surface_t *surface)
{
    /* Set the coordinate transform */
    xcairo_set_matrix (context, matrix);
    
    /* Now copy it to the screen */
    xcairo_set_source_surface (context, surface, 0, 0);
    xcairo_paint (context);
    
    /* Restore the coordinate system to normal */
    xcairo_identity_matrix (context);
}


void Background::Update_layer_from_cairo_surface (cairo_surface_t *layer, int z_level)
{
    SDL_assert(layer != nullptr);
    SDL_assert(z_level >= 0 && z_level < MAIN_BACKGROUNDS_COUNT);
    
    if (layers[z_level].surface != nullptr) {
        xcairo_surface_destroy(layers[z_level].surface);
    }
    if (layer) {
        layers[z_level].surface = layer;
        xcairo_surface_reference(layer);
    }
    else
        layers[z_level].surface = NULL;
}

void Background::Update_layers_from_tmx_map(Map& map, ResCache& resource_cache)
{
    for (int z_level = 0; z_level < 4; z_level ++) {
        cairo_surface_t *surface = tmx_render_layer_to_cairo_surface(map, z_level, resource_cache);
        Update_layer_from_cairo_surface(surface, z_level);
        xcairo_surface_destroy(surface);
    }
}

void
Background::Draw_zlevel_to_context(cairo_t *screen, int z_level)
{
    if(layers[z_level].enable) {
        cairo_matrix_t matrix = compute_transform (layers[z_level].scroll_x,
                                                   layers[z_level].scroll_y,
                                                   layers[z_level].rotation_center_x,
                                                   layers[z_level].rotation_center_y,
                                                   layers[z_level].rotation,
                                                   layers[z_level].expansion);
        paint_transformed_image (screen, &matrix, layers[z_level].surface);
        
    }
}

void Background::Reset()
{
    colorswap = false;
    brightness = 1.0;
    for (auto i = 0u; i < layers.size(); i ++) {
        layers[i].scroll_x = 0.0;
        layers[i].scroll_y = 0.0;
        layers[i].rotation_center_x = 0.0;
        layers[i].rotation_center_y = 0.0;
        layers[i].expansion = 1.0;
        layers[i].rotation = 0.0;
    }
}
