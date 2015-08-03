#include "Background.hpp"
#include "xcairo.hpp"
#include "ResCache.hpp"
#include "Tmx.h"
#include "tmx.hpp"
#include "xsdl.hpp"
#include "ResCache.hpp"

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
                                     cairo_surface_t *surface,
                                     double alpha)
{
    /* Set the coordinate transform */
    xcairo_set_matrix (context, matrix);
    
    /* Now copy it to the screen */
    xcairo_set_source_surface (context, surface, 0, 0);
    xcairo_set_antialias (context, CAIRO_ANTIALIAS_NONE);

    cairo_paint_with_alpha (context, alpha);
    
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
        Update_layer_from_cairo_surface(surface, 3 - z_level);
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
        paint_transformed_image (screen, &matrix, layers[z_level].surface, layers[z_level].opacity);
        
    }
}

void Background::Reset()
{
    for (auto i = 0u; i < layers.size(); i ++) {
        layers[i].enable = true;
        layers[i].scroll_x = 0.0;
        layers[i].scroll_y = 0.0;
        layers[i].rotation_center_x = 0.0;
        layers[i].rotation_center_y = 0.0;
        layers[i].expansion = 1.0;
        layers[i].rotation = 0.0;
        layers[i].opacity = 1.0;
    }
}

void
bg_reset()
{
    bg.Reset();
}
void bg_set_opacity (int i, double val)
{
    bg.layers[i].opacity = val;
}

double bg_get_opacity (int i)
{
    return bg.layers[i].opacity;
}

void bg_hide(int i)
{
    bg.layers[i].enable = false;
}

void bg_show(int i)
{
    bg.layers[i].enable = true;
}

bool bg_is_shown(int i)
{
    return bg.layers[i].enable;
}

void bg_rotate (int id, double angle)
{
    bg.layers[id].rotation += angle;
}

void bg_scroll (int id, double dx, double dy)
{
    bg.layers[id].scroll_x += dx;
    bg.layers[id].scroll_y += dy;
}

void bg_set (int id, double rotation, double expansion, double scroll_x,
             double scroll_y, double rotation_center_x, double rotation_center_y,
             double opacity)
{
    bg.layers[id].rotation = rotation;
    bg.layers[id].expansion = expansion;
    bg.layers[id].scroll_x = scroll_x;
    bg.layers[id].scroll_y = scroll_y;
    bg.layers[id].rotation_center_x = rotation_center_x;
    bg.layers[id].rotation_center_y = rotation_center_y;
    bg.layers[id].opacity = opacity;
}

void
bg_set_scroll (int id, double scroll_x, double scroll_y)
{
    bg.layers[id].scroll_x = scroll_x;
    bg.layers[id].scroll_y = scroll_y;
}

void
bg_set_rotation_center (int id, double rotation_center_x, double rotation_center_y)
{
    bg.layers[id].rotation_center_x = rotation_center_x;
    bg.layers[id].rotation_center_y = rotation_center_y;
}

void
bg_set_rotation (int id, double rotation)
{
    bg.layers[id].rotation = rotation;
}

void
bg_set_rotation_expansion (int id, double rotation, double expansion)
{
  bg.layers[id].rotation = rotation;
  bg.layers[id].expansion = expansion;
}

void
bg_set_expansion (int id, double expansion)
{
  bg.layers[id].expansion = expansion;
}

void bg_update_from_tmx_map(void)
{
    bg.Update_layers_from_tmx_map(tmx_map, resource_cache);
}

DEFINE_VOID_FUNC_VOID(bgReset, bg_reset);
DEFINE_VOID_FUNC_INT_DOUBLE(bgSetOpacity, bg_set_opacity);
DEFINE_DOUBLE_FUNC_INT(bgGetOpacity, bg_get_opacity);
DEFINE_VOID_FUNC_INT(bgHide, bg_hide);
DEFINE_VOID_FUNC_INT(bgShow, bg_show);
DEFINE_BOOL_FUNC_INT(bgIsShown, bg_is_shown);
DEFINE_VOID_FUNC_INT_DOUBLE(bgRotate, bg_rotate);
DEFINE_VOID_FUNC_INT_DOUBLEx2(bgScroll, bg_scroll);
DEFINE_VOID_FUNC_INT_DOUBLEx7(bgSet, bg_set);
DEFINE_VOID_FUNC_INT_DOUBLEx2(bgSetScroll, bg_set_scroll);
DEFINE_VOID_FUNC_INT_DOUBLEx2(bgSetRotationCenter, bg_set_rotation_center);
DEFINE_VOID_FUNC_INT_DOUBLE(bgSetRotation, bg_set_rotation);
DEFINE_VOID_FUNC_INT_DOUBLEx2(bgSetRotationExpansion, bg_set_rotation_expansion);
DEFINE_VOID_FUNC_INT_DOUBLE(bgSetExpansion, bg_set_expansion);

DEFINE_VOID_FUNC_VOID(bgUpdateFromTmxMap, bg_update_from_tmx_map);

JSFunctionSpec background_functions[16] = {
    DECLARE_VOID_FUNC_VOID(bgReset, bg_reset)
    DECLARE_VOID_FUNC_INT_DOUBLE(bgSetOpacity, bg_set_opacity)
    DECLARE_DOUBLE_FUNC_INT(bgGetOpacity, bg_get_opacity)
    DECLARE_VOID_FUNC_INT(bgHide, bg_hide)
    DECLARE_VOID_FUNC_INT(bgShow, bg_show)
    DECLARE_BOOL_FUNC_INT(bgIsShown, bg_is_shown)
    DECLARE_VOID_FUNC_INT_DOUBLE(bgRotate, bg_rotate)
    DECLARE_VOID_FUNC_INT_DOUBLEx2(bgScroll, bg_scroll)
    DECLARE_VOID_FUNC_INT_DOUBLEx7(bgSet, bg_set)
    DECLARE_VOID_FUNC_INT_DOUBLEx2(bgSetScroll, bg_set_scroll)
    DECLARE_VOID_FUNC_INT_DOUBLEx2(bgSetRotationCenter, bg_set_rotation_center)
    DECLARE_VOID_FUNC_INT_DOUBLE(bgSetRotation, bg_set_rotation)
    DECLARE_VOID_FUNC_INT_DOUBLEx2(bgSetRotationExpansion, bg_set_rotation_expansion)
    DECLARE_VOID_FUNC_INT_DOUBLE(bgSetExpansion, bg_set_expansion)
    DECLARE_VOID_FUNC_VOID(bgUpdateFromTmxMap, bg_update_from_tmx_map)
    JS_FS_END
};

