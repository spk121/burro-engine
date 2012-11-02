#include <stddef.h>  // NULL
#include <cairo.h>
#include <math.h>
#include "engine.h"
#include "eng_video.h"



static uint32_t adjust_colorval (uint32_t colorval, double brightness, _Bool color_swap);
static void draw_backdrop_color(void);
static void draw_background_layer (const struct bg_entry * const bg, cairo_t * const screen_context);
static void draw_background_map_layer (const struct bg_entry * const bg, cairo_t * const screen_context);
static void draw_background_indexed_bitmap_layer (const struct bg_entry * const bg, cairo_t * const screen_context);
static void draw_background_true_color_bitmap_layer (const struct bg_entry * const bg, cairo_t * const screen_context);
static void finalize_context_and_surface (cairo_t *context, cairo_surface_t *surface);
static void initialize_context_and_surface (cairo_t **context, cairo_surface_t **surface, int width, int height);
static void paint_backdrop_color (cairo_t *context, uint32_t r, uint32_t g, uint32_t b);
static void paint_transformed_image (cairo_t *context, cairo_matrix_t *m, cairo_surface_t *surface);
static void unpack_colorval (uint32_t colorval, double brightness, _Bool color_swap, uint32_t *r, uint32_t *g, uint32_t *b, uint32_t *a);

void initialize_video()
{
    int i;

    for (i = 0; i < MAIN_BACKGROUNDS_COUNT; i ++)
        e.main_bg[i].expansion = 1.0;
    for(i = 0; i < MAIN_SPRITES_COUNT; i++)
        e.main_obj[i].expansion = 1.0;
    for (i = 0; i < SUB_BACKGROUNDS_COUNT; i ++)
        e.sub_bg[i].expansion = 1.0;
    for(i = 0; i < SUB_SPRITES_COUNT; i++)
        e.sub_obj[i].expansion = 1.0;

    initialize_context_and_surface (&e.priv.main_screen_context, &e.priv.main_screen_surface, MAIN_SCREEN_WIDTH_IN_PIXELS, MAIN_SCREEN_HEIGHT_IN_PIXELS);
    initialize_context_and_surface (&e.priv.sub_screen_context, &e.priv.sub_screen_surface, SUB_SCREEN_WIDTH_IN_PIXELS, SUB_SCREEN_HEIGHT_IN_PIXELS);
}

void fini_draw ()
{
    finalize_context_and_surface (e.priv.main_screen_context, e.priv.main_screen_surface);
    finalize_context_and_surface (e.priv.sub_screen_context, e.priv.sub_screen_surface);
}

/*************************************************************************
 * STATIC FUNCTIONS
 *************************************************************************/

static uint32_t adjust_colorval (uint32_t colorval, double brightness, _Bool color_swap)
{
    uint32_t r, g, b, a;
    unpack_colorval(colorval, brightness, color_swap, &r, &g, &b, &a);
    return RGBA(r, g, b, a);
}

static void compute_transform (cairo_matrix_t *matrix,
                               double rotation_center_screen_x, double rotation_center_screen_y,
                               int rotation_center_bitmap_row, int rotation_center_bitmap_column,
                               double rotation_angle, double expansion_factor)
{
    double xx, xy, yx, yy, x0, y0;
    double sn, cs;
    if (expansion_factor == 0.0)
        expansion_factor = 1.0;
    sn = sin (rotation_angle);
    cs = cos (rotation_angle);
    xx = expansion_factor * cs;
    xy = expansion_factor * sn;
    yx = -xy;
    yy = xx;
    x0 = rotation_center_screen_x - (xx * (double)rotation_center_bitmap_column + xy * (double) rotation_center_bitmap_row);
    y0 = rotation_center_screen_y - (yx * (double)rotation_center_bitmap_column + yy * (double) rotation_center_bitmap_row);
    matrix->xx = xx;
    matrix->xy = xy;
    matrix->yx = yx;
    matrix->yy = yy;
    matrix->x0 = x0;
    matrix->y0 = y0;
}

static void draw_backdrop_color()
{
    uint32_t r, g, b, a;

    unpack_colorval(e.bg_color, e.brightness, e.color_swap, &r, &g, &b, &a);
    paint_backdrop_color (e.priv.main_screen_context, r, g, b);
    paint_backdrop_color (e.priv.sub_screen_context, r, g, b);
}

static void draw_background_layer (const struct bg_entry * const bg, cairo_t * const screen_context)
{
    switch (bg->mode)
    {
    case BG_MODE_TILE_AND_MAP:
        draw_background_map_layer (bg, screen_context);
        break;
    case BG_MODE_INDEXED_BITMAP:
        draw_background_indexed_bitmap_layer (bg, screen_context);
        break;
    case BG_MODE_TRUE_COLOR_BITMAP:
        draw_background_true_color_bitmap_layer (bg, screen_context);
        break;
    default:
        break;
    }
}

static void finalize_context_and_surface (cairo_t *context, cairo_surface_t *surface)
{
    cairo_destroy (context);
    cairo_surface_destroy (surface);
}

static void initialize_context_and_surface (cairo_t **context, cairo_surface_t **surface, int width, int height)
{
    *surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
    *context = cairo_create (*surface);
    cairo_set_antialias (*context, CAIRO_ANTIALIAS_NONE);
}

static void paint_backdrop_color (cairo_t *context, uint32_t r, uint32_t g, uint32_t b)
{
    double dr, db, dg;

    dr = (double) r / 255.0;
    dg = (double) g / 255.0;
    db = (double) b / 255.0;
    cairo_set_source_rgb (context, dr, dg, db);
    cairo_paint (context);
}

static void paint_transformed_image (cairo_t *context, cairo_matrix_t *matrix, cairo_surface_t *surface)
{
    /* Set the coordinate transform */
    cairo_set_matrix (context, matrix);

    /* Now copy it to the screen */
    cairo_set_source_surface (context, surface, 0, 0);
    cairo_paint (context);

    /* Restore the coordinate system to normal */
    cairo_identity_matrix(context);
}

static void unpack_colorval (uint32_t colorval, double brightness, _Bool color_swap, uint32_t *r, uint32_t *g, uint32_t *b, uint32_t *a)
{
    *r = RGBA_TO_RED(colorval) * brightness;
    *g = RGBA_TO_GREEN(colorval) * brightness;
    *b = RGBA_TO_BLUE(colorval) * brightness;
    *a = RGBA_TO_ALPHA(colorval);
    if (color_swap)
    {
        uint32_t tmp = *r;
        *r = *b;
        *b = tmp;
    }
}

static void draw_background_map_layer (const struct bg_entry * const bg, cairo_t * const screen_context)
{
    cairo_surface_t *surf;
    uint32_t *data;
    int stride;
    int map_j, map_i;
    int tile_j, tile_i, delta_tile_i, delta_tile_j;
    int map_index;
    uint32_t index, c;
    cairo_matrix_t matrix;

    /* Make the cairo surface */
    surf = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, TILE_WIDTH_IN_PIXELS, TILE_HEIGHT_IN_PIXELS);
    data = (uint32_t *) cairo_image_surface_get_data (surf);
    stride = cairo_image_surface_get_stride (surf);

    for (map_j = 0; map_j < bg->map.map_height_in_tiles; map_j ++)
    {
        for (map_i = 0; map_i < bg->map.map_width_in_tiles; map_i ++)
        {
            /* Fill in the tile brush */
            cairo_surface_flush (surf);
            map_index = bg->map.map[map_j][map_i];
            delta_tile_j = (map_index / TILESHEET_WIDTH_IN_TILES) * TILE_HEIGHT_IN_PIXELS;
            delta_tile_i = (map_index % TILESHEET_WIDTH_IN_TILES) * TILE_WIDTH_IN_PIXELS;
            for (tile_j = 0; tile_j < TILE_HEIGHT_IN_PIXELS; tile_j ++)
            {
                for (tile_i = 0; tile_i < TILE_WIDTH_IN_PIXELS; tile_i ++)
                {
                    index = bg->map.tiles[delta_tile_j + tile_j][delta_tile_i + tile_i];

                    /* For Burro, palette index zero is always transparent */
                    if (index == 0)
                        data[tile_j * stride / sizeof(uint32_t) + tile_i] = 0;
                    else
                    {
                        c = adjust_colorval (bg->map.palette[index], e.brightness, e.color_swap);
                        data[tile_j * stride / sizeof(uint32_t) + tile_i] = c;
                    }
                }
            }
            cairo_surface_mark_dirty (surf);
            compute_transform (&matrix, bg->center_x, bg->center_y,
                               bg->center_i - tile_i * TILESHEET_WIDTH_IN_PIXELS,
                               bg->center_j - tile_j * TILESHEET_HEIGHT_IN_PIXELS,
                               bg->rotation, bg->expansion);
            paint_transformed_image (screen_context, &matrix, surf);
        }
    }
    cairo_surface_destroy (surf);
}

static void draw_background_indexed_bitmap_layer (const struct bg_entry * const bg, cairo_t * const screen_context)
{
    int j, stride, i;
    cairo_surface_t *surf;
    uint32_t *data;
    uint32_t index, c;
    cairo_matrix_t matrix;

    /* First, make a cairo surface */
    surf = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, bg->bmp8.width_in_pixels, bg->bmp8.height_in_pixels);
    data = (uint32_t *) cairo_image_surface_get_data (surf);
    stride = cairo_image_surface_get_stride (surf);
    cairo_surface_flush (surf);
    for (j = 0; j < bg->bmp8.height_in_pixels; j++)
    {
        for (i = 0; i < bg->bmp8.width_in_pixels; i++)
        {
            index = bg->bmp8.bmp[j][i];
            /* For Burro, palette index zero is always transparent */
            if (index == 0)
            {
                data[j * stride / sizeof(uint32_t) + i] = 0;
            }
            else
            {
                c = adjust_colorval (bg->bmp8.palette[index], e.brightness, e.color_swap);
                data[j * stride / sizeof(uint32_t) + i] = c;
            }
        }
    }

    /* Now copy it to the screen */
    cairo_surface_mark_dirty (surf);
    compute_transform (&matrix, bg->center_x, bg->center_y,
                       bg->center_i, bg->center_j,
                       bg->rotation, bg->expansion);
    paint_transformed_image (screen_context, &matrix, surf);
    cairo_surface_destroy (surf);
}

static void draw_background_true_color_bitmap_layer (const struct bg_entry * const bg, cairo_t * const screen_context)
{
    int j, stride, i;
    cairo_surface_t *surf;
    uint32_t *data;
    uint32_t c;
    cairo_matrix_t matrix;

    /* First, make a cairo surface */
    surf = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, bg->bmp32.width_in_pixels, bg->bmp32.height_in_pixels);
    data = (uint32_t *) cairo_image_surface_get_data (surf);
    stride = cairo_image_surface_get_stride (surf);
    cairo_surface_flush (surf);
    for (j = 0; j < bg->bmp32.height_in_pixels; j++)
    {
        for (i = 0; i < bg->bmp32.width_in_pixels; i++)
        {
            c = adjust_colorval(bg->bmp32.bmp[j][i], e.brightness, e.color_swap);
            data[j * stride / sizeof(uint32_t) + i] = c;
        }
    }

    /* Now copy it to the screen */
    cairo_surface_mark_dirty (surf);
    compute_transform (&matrix, bg->center_x, bg->center_y,
                       bg->center_i, bg->center_j,
                       bg->rotation, bg->expansion);
    paint_transformed_image (screen_context, &matrix, surf);
    cairo_surface_destroy (surf);
}

static void draw_sprite (struct obj_entry *obj, struct obj_data *spritesheet, cairo_t *screen_context)
{
    cairo_surface_t *surf;
    uint32_t *data;
    int stride;
    int sprite_j, sprite_i;
    uint32_t index, c;
    cairo_matrix_t matrix;

    /* Make the cairo surface */
    surf = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, obj->sprite_width, obj->sprite_height);
    data = (uint32_t *) cairo_image_surface_get_data (surf);
    stride = cairo_image_surface_get_stride (surf);
    cairo_surface_flush (surf);
    for (sprite_j = 0; sprite_j < obj->sprite_height; sprite_j ++)
    {
        for (sprite_i = 0; sprite_i < obj->sprite_width; sprite_i ++)
        {
            /* Fill in the tile brush */
            index = spritesheet->bmp[obj->spritesheet_j + sprite_j][obj->spritesheet_i + sprite_i];

            /* For Burro, palette index zero is always transparent */
            if (index == 0)
                data[sprite_j * stride / sizeof(uint32_t) + sprite_i] = 0;
            else
            {
                c = adjust_colorval(spritesheet->palette[index], e.brightness, e.color_swap);
                data[sprite_j * stride / sizeof(uint32_t) + sprite_i] = c;
            }
        }
    }
    cairo_surface_mark_dirty (surf);

    /* Set the paintbrush-to-screen coordinate transform */
    compute_transform (&matrix, obj->center_x, obj->center_y, obj->center_i, obj->center_j, obj->rotation, obj->expansion);
    paint_transformed_image(screen_context, &matrix, surf);
    cairo_surface_destroy (surf);
}

void draw ()
{
    int priority;
    int layer, sprite;

    cairo_surface_flush (e.priv.main_screen_surface);
    cairo_surface_flush (e.priv.sub_screen_surface);

    /* blank the screens to a solid color */
    draw_backdrop_color ();
    if (e.blank)
        goto end_draw;

    for (priority = PRIORITY_COUNT - 1; priority >= 0; priority --)
    {
        for (layer = MAIN_BACKGROUNDS_COUNT - 1; layer >= 0; layer --)
        {
            if (e.main_bg[layer].enable && e.main_bg[layer].priority == priority)
                draw_background_layer (&(e.main_bg[layer]), e.priv.main_screen_context);
        }
        for (layer = SUB_BACKGROUNDS_COUNT - 1; layer >= 0; layer --)
        {
            if (e.sub_bg[layer].enable && e.sub_bg[layer].priority == priority)
                draw_background_layer (&(e.sub_bg[layer]), e.priv.sub_screen_context);
        }
        for (sprite = MAIN_SPRITES_COUNT; sprite >= 0; sprite --)
        {
            if (e.main_obj[sprite].enable && e.main_obj[sprite].priority == priority)
                draw_sprite(&(e.main_obj[sprite]), &(e.main_objsheet), e.priv.main_screen_context);
        }
        for (sprite = SUB_SPRITES_COUNT; sprite >= 0; sprite --)
        {
            if (e.sub_obj[sprite].enable && e.sub_obj[sprite].priority == priority)
                draw_sprite(&(e.main_obj[sprite]), &(e.main_objsheet), e.priv.main_screen_context);

        }
    }

end_draw:
    cairo_surface_mark_dirty (e.priv.main_screen_surface);
    cairo_surface_mark_dirty (e.priv.sub_screen_surface);
}

