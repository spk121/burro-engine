#include <stddef.h>  // NULL
#include <cairo.h>
#include "engine.h"
#include "eng_draw.h"

cairo_t *m_main_screen_context;
cairo_surface_t *m_main_screen_surface;
int m_main_screen_stride;
uint32_t *m_main_screen_data;

cairo_t *m_sub_screen_context;
cairo_surface_t *m_sub_screen_surface;
int m_sub_screen_stride;
uint32_t *m_sub_screen_data;

static void draw_backdrop_color(void);
static void draw_background_layer (struct bg_entry *bg, cairo_t *screen_context);
static void draw_background_map_layer (struct bg_entry *bg, cairo_t *screen_context);
static void draw_background_indexed_bitmap_layer (struct bg_entry *bg, cairo_t *screen_context);
static void draw_background_true_color_bitmap_layer (struct bg_entry *bg, cairo_t *screen_context);


void init_draw ()
{
    m_main_screen_surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, MAIN_SCREEN_WIDTH_IN_PIXELS, MAIN_SCREEN_HEIGHT_IN_PIXELS);
    m_main_screen_stride = cairo_image_surface_get_stride (m_main_screen_surface);
    m_main_screen_data = (uint32_t *) cairo_image_surface_get_data (m_main_screen_surface);

    m_sub_screen_surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, SUB_SCREEN_WIDTH_IN_PIXELS, SUB_SCREEN_HEIGHT_IN_PIXELS);
    m_sub_screen_stride = cairo_image_surface_get_stride (m_sub_screen_surface);
    m_sub_screen_data = (uint32_t *) cairo_image_surface_get_data (m_sub_screen_surface);

    m_main_screen_context = cairo_create (m_main_screen_surface);
    cairo_set_antialias (m_main_screen_context, CAIRO_ANTIALIAS_NONE);

    m_sub_screen_context = cairo_create (m_sub_screen_surface);
    cairo_set_antialias (m_sub_screen_context, CAIRO_ANTIALIAS_NONE);
}

void fini_draw ()
{
    cairo_destroy (m_main_screen_context);
    cairo_surface_destroy (m_main_screen_surface);
    m_main_screen_surface = NULL;
    m_main_screen_stride = 0;
    m_main_screen_data = NULL;

    cairo_destroy (m_sub_screen_context);
    cairo_surface_destroy (m_sub_screen_surface);
    m_sub_screen_surface = NULL;
    m_sub_screen_stride = 0;
    m_sub_screen_data = NULL;

}

static void draw_backdrop_color()
{
    if (!e.color_swap)
    {
        cairo_set_source_rgb (m_main_screen_context, RGBA_TO_RED_RATIO(e.bg_color) * e.brightness,
                              RGBA_TO_GREEN_RATIO(e.bg_color) * e.brightness,
                              RGBA_TO_BLUE_RATIO(e.bg_color) * e.brightness);
        cairo_paint (m_main_screen_context);

        cairo_set_source_rgb (m_sub_screen_context, RGBA_TO_RED_RATIO(e.bg_color) * e.brightness,
                              RGBA_TO_GREEN_RATIO(e.bg_color) * e.brightness,
                              RGBA_TO_BLUE_RATIO(e.bg_color) * e.brightness);
        cairo_paint (m_sub_screen_context);
    }
    else
    {
        cairo_set_source_rgb (m_main_screen_context, RGBA_TO_BLUE_RATIO(e.bg_color) * e.brightness,
                              RGBA_TO_GREEN_RATIO(e.bg_color) * e.brightness,
                              RGBA_TO_RED_RATIO(e.bg_color) * e.brightness);
        cairo_paint (m_main_screen_context);

        cairo_set_source_rgb (m_sub_screen_context, RGBA_TO_BLUE_RATIO(e.bg_color) * e.brightness,
                              RGBA_TO_GREEN_RATIO(e.bg_color) * e.brightness,
                              RGBA_TO_RED_RATIO(e.bg_color) * e.brightness);
        cairo_paint (m_sub_screen_context);
    }
}

static void draw_background_layer (struct bg_entry *bg, cairo_t *screen_context)
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

static void set_context_matrix_from_bg (cairo_matrix_t *m, struct bg_entry *bg)
{
    m->xx = bg->xx;
    m->xy = bg->xy;
    m->yx = bg->yx;
    m->yy = bg->yy;
    m->x0 = bg->x0;
    m->y0 = bg->y0;
}

static void draw_background_map_layer (struct bg_entry *bg, cairo_t *screen_context)
{
    cairo_surface_t *surf;
    uint32_t *data;
    int stride;
    int map_j, map_i;
    int tile_j, tile_i, delta_tile_i, delta_tile_j;
    int map_index;
    uint32_t index, c, r, g, b, a;
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
                    {
                        data[tile_j * stride / sizeof(uint32_t) + tile_i] = 0;
                    }
                    else
                    {
                        c = bg->map.palette[index];
                        r = RGBA_TO_RED(c) * e.brightness;
                        g = RGBA_TO_GREEN(c) * e.brightness;
                        b = RGBA_TO_BLUE(c) * e.brightness;
                        a = RGBA_TO_ALPHA(c);
                        if (e.color_swap)
                        {
                            uint32_t tmp;
                            tmp = r;
                            r = b;
                            b = tmp;
                        }
                        data[tile_j * stride / sizeof(uint32_t) + tile_i] = r | g << 8 | b << 16 | a << 24;
                    }
                }
            }
            /* Now copy it to the screen */
            cairo_surface_mark_dirty (surf);
            /* FIXME: this translation isn't right */
            set_context_matrix_from_bg(&matrix, bg);
            cairo_set_matrix (screen_context, &matrix);
            cairo_translate (screen_context, tile_i * TILESHEET_WIDTH_IN_PIXELS, tile_j * TILESHEET_HEIGHT_IN_PIXELS);
            cairo_set_source_surface (screen_context, surf, 0, 0);
            cairo_paint (screen_context);
        }
    }
    cairo_surface_destroy (surf);
}

static void draw_background_indexed_bitmap_layer (struct bg_entry *bg, cairo_t *screen_context)
{
    int j, stride, i;
    cairo_surface_t *surf;
    uint32_t *data;
    uint32_t index, c, r, g, b, a;
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
                c = bg->bmp8.palette[index];
                r = RGBA_TO_RED(c) * e.brightness;
                g = RGBA_TO_GREEN(c) * e.brightness;
                b = RGBA_TO_BLUE(c) * e.brightness;
                a = RGBA_TO_ALPHA(c);
                if (e.color_swap)
                {
                    uint32_t tmp;
                    tmp = r;
                    r = b;
                    b = tmp;
                }
                data[j * stride / sizeof(uint32_t) + i] = r | g << 8 | b << 16 | a << 24;
            }
        }
    }

    /* Now copy it to the screen */
    cairo_surface_mark_dirty (surf);
    set_context_matrix_from_bg(&matrix, bg);
    cairo_set_matrix (screen_context, &matrix);
    cairo_set_source_surface (screen_context, surf, 0, 0);
    cairo_paint (screen_context);
    cairo_surface_destroy (surf);
}

static void draw_background_true_color_bitmap_layer (struct bg_entry *bg, cairo_t *screen_context)
{
    int j, stride, i;
    cairo_surface_t *surf;
    uint32_t *data;
    uint32_t c, r, g, b, a;
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
            c = bg->bmp32.bmp[j][i];
            r = RGBA_TO_RED(c) * e.brightness;
            g = RGBA_TO_GREEN(c) * e.brightness;
            b = RGBA_TO_BLUE(c) * e.brightness;
            a = RGBA_TO_ALPHA(c);
            if (e.color_swap)
            {
                uint32_t tmp;
                tmp = r;
                r = b;
                b = tmp;
            }
            data[j * stride / sizeof(uint32_t) + i] = r | g << 8 | b << 16 | a << 24;
        }
    }

    /* Now copy it to the screen */
    cairo_surface_mark_dirty (surf);
    set_context_matrix_from_bg (&matrix, bg);
    cairo_set_matrix (screen_context, &matrix);
    cairo_set_source_surface (screen_context, surf, 0, 0);
    cairo_paint (screen_context);
    cairo_surface_destroy (surf);
}

void draw ()
{
    int priority;
    int layer, sprite;

    /* blank the screens to a solid color */
    draw_backdrop_color ();
    if (e.blank)
        return;

    for (priority = PRIORITY_COUNT - 1; priority >= 0; priority --)
    {
        for (layer = MAIN_BACKGROUNDS_COUNT - 1; layer >= 0; layer --)
        {
            if (e.main_bg[layer].enable && e.main_bg[layer].priority == priority)
                draw_background_layer (&(e.main_bg[layer]), m_main_screen_context);
        }
        for (layer = SUB_BACKGROUNDS_COUNT - 1; layer >= 0; layer --)
        {
            if (e.sub_bg[layer].enable && e.sub_bg[layer].priority == priority)
                draw_background_layer (&(e.sub_bg[layer]), m_sub_screen_context);
        }
        for (sprite = MAIN_SPRITES_COUNT; sprite >= 0; sprite --)
        {
            if (e.main_obj[sprite].enable && e.main_obj[sprite].priority == priority)
                //draw_sprite(e.main_obj[sprite])
                ;
        }
        for (sprite = SUB_SPRITES_COUNT; sprite >= 0; sprite --)
        {
            if (e.sub_obj[sprite].enable && e.sub_obj[sprite].priority == priority)
                //draw_sprite(e.sub_obj[sprite])
                ;
        }
    }
}

