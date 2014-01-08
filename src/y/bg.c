#include "../x/xcairo.h"
#include "bg.h"
#include "eng.h"
#include "tga.h"
//#include "engine.h"

struct bg_map_data
{
  int height;
  int width;
  guint16 map[MAP_HEIGHT][MAP_WIDTH];
  guint8 tilesheet[TILESHEET_HEIGHT][TILESHEET_WIDTH];
  guint16 palette[PALETTE_COLORS_COUNT];
};


struct bg_bmp8_data
{
  int height;
  int width;
  guint8 bmp[BMP8_HEIGHT][BMP8_WIDTH];
  guint16 palette[PALETTE_COLORS_COUNT];
};

struct bg_bmp16_data
{
  int height;
  int width;
  guint16 bmp[BMP16_HEIGHT][BMP16_WIDTH];
};


typedef struct bg_entry
{
  /* BG is display when true */
  gboolean enable;

  /** tile and map, palette bmp, or true color bmp */
  bg_type_t type;

  /* z-level: 0 is foreground, 3 is background */
  int priority;

  /** the "user" or screen location of the rotation center of the background */
  double scroll_x, scroll_y;

  /** the "device" location of the rotation center of the background*/
  double rotation_center_x, rotation_center_y;

  /** the expansion factor of the background: 1.0 = 1 pixel per pixel */
  double expansion;

  /** the rotation angle of the background about its rotation center, in radians */
  double rotation;

  union
  {
    struct bg_map_data map;
    struct bg_bmp8_data bmp8;
    struct bg_bmp16_data bmp16;
  };
} bg_entry_t;

typedef struct bg_tag
{
  /* The RGBA color displayed below all backgrounds and sprites */
  guint16 bg_color;

  bg_entry_t bg[MAIN_BACKGROUNDS_COUNT + SUB_BACKGROUNDS_COUNT];
} bg_t;

bg_t bg;

static cairo_surface_t *
bg_render_map_to_cairo_surface (int id);
static cairo_surface_t *
bg_render_bmp8_to_cairo_surface (int id);
static cairo_surface_t *
bg_render_bmp16_to_cairo_surface (int id);


static guint32
adjust_colorval (guint16 c16)
{
  guint32 a, r, g, b, c32;

  a = (((guint32) c16 & 0b1000000000000000) >> 15);
  r = (((guint32) c16 & 0b0111110000000000) >> 10);
  g = (((guint32) c16 & 0b0000001111100000) >> 5);
  b = ((guint32) c16 & 0b0000000000011111);

  if (a == 1 || (r == 0x1f && g == 0x0 && b == 0x1f))
    {
      /* Transparent */
      return 0x0;
    }

  if (eng_is_colorswap ())
    {
      double temp = r;
      r = b;
      b = temp;
    } 
  a = 0xff;
  r = r * eng_get_brightness ();
  g = g * eng_get_brightness ();
  b = b * eng_get_brightness ();
  /* Add 3 extra bits to promote the 5-bit val to 8-bit val */
  c32 = (a << 24) + (r << 19) + (g << 11) + (b << 3);
  return c32;
}

void bg_set_backdrop_color (guint16 c16)
{
  bg.bg_color = c16;
}

void bg_get_backdrop_color_rgb (double *r, double *g, double *b)
{
  guint32 c32 = adjust_colorval (bg.bg_color);
  *r = ((double)((c32 & 0x00ff0000) >> 16)) / 255.0;
  *g = ((double)((c32 & 0x0000ff00) >> 8)) / 255.0;
  *b = ((double)((c32 & 0x000000ff))) / 255.0;
}


gboolean bg_is_shown (int id)
{
  return bg.bg[id].enable;
}


guint16 *bg_get_map_ptr (int id)
{
  return &(bg.bg[id].map.map[0][0]);
}

guint8 *bg_get_tilesheet_ptr (int id)
{
  return &(bg.bg[id].map.tilesheet[0][0]);
}

guint8 *bg_get_bmp8_ptr (int id)
{
  return &(bg.bg[id].bmp8.bmp[0][0]);
}

guint16 *bg_get_bmp16_ptr (int id)
{
  return &(bg.bg[id].bmp16.bmp[0][0]);
}

int bg_get_priority (int id)
{
  return bg.bg[id].priority;
}

void bg_hide (int id)
{
  bg.bg[id].enable = FALSE;
}

void bg_init (int id, bg_type_t type, guint width, guint height)
{
  bg.bg[id].type = type;
  bg.bg[id].scroll_x = 0.0;
  bg.bg[id].scroll_y = 0.0;
  bg.bg[id].rotation_center_x = 0.0;
  bg.bg[id].rotation_center_y = 0.0;
  bg.bg[id].expansion = 1.0;
  bg.bg[id].rotation = 0.0;
  switch (type)
    {
    case BG_TYPE_MAP:
      bg.bg[id].map.height = height;
      bg.bg[id].map.width = width;
      break;
    case BG_TYPE_BMP8:
      bg.bg[id].bmp8.height = height;
      bg.bg[id].bmp8.width = width;
      break;
    case BG_TYPE_BMP16:
      bg.bg[id].bmp16.height = height;
      bg.bg[id].bmp16.width = width;
      break;
    }
}

void bg_rotate (int id, double angle)
{
  bg.bg[id].rotation += angle;
}

void bg_scroll (int id, double dx, double dy)
{
  bg.bg[id].scroll_x += dx;
  bg.bg[id].scroll_y += dy;
}

void bg_set (int id, double rotation, double expansion, double scroll_x, double scroll_y, 
	     double rotation_center_x, double rotation_center_y)
{
  bg.bg[id].rotation = rotation;
  bg.bg[id].expansion = expansion;
  bg.bg[id].scroll_x = scroll_x;
  bg.bg[id].scroll_y = scroll_y;
  bg.bg[id].rotation_center_x = rotation_center_x;
  bg.bg[id].rotation_center_y = rotation_center_y;
}

void bg_set_rotation_center (int id, double rotation_center_x, double rotation_center_y)
{
  bg.bg[id].rotation_center_x = rotation_center_x;
  bg.bg[id].rotation_center_y = rotation_center_y;
}

void bg_set_priority (int id, int priority)
{
  bg.bg[id].priority = priority;
}

void bg_set_rotation (int id, double rotation)
{
  bg.bg[id].rotation = rotation;
}

void bg_set_rotation_expansion (int id, double rotation, double expansion)
{
  bg.bg[id].rotation = rotation;
  bg.bg[id].expansion = expansion;
}

void bg_set_expansion (int id, double expansion)
{
  bg.bg[id].expansion = expansion;
}

void bg_show (int id)
{
  bg.bg[id].enable = TRUE;
}

void bg_set_map_from_tga (int id, targa_image_t *t)
{
  guint width, height;
  targa_get_image_dimensions (t, &width, &height);
  bg.bg[id].map.width = width;
  bg.bg[id].map.height = height;
  for (guint j = 0; j < height; j ++)
    {
      for (guint i = 0; i < width ; i ++)
	{
	  bg.bg[id].map.map[j][i] = tga_get_image_data_u16_ptr(t)[j * width + i];
	}
    }
}

void bg_set_tilesheet_from_tga (int id, targa_image_t *t)
{
  guint width, height;
  int first = targa_get_color_map_first_index (t);

  targa_get_image_dimensions (t, &width, &height);

  for (guint j = 0; j < height; j ++)
    {
      for (guint i = 0; i < width ; i ++)
	{
	  bg.bg[id].map.tilesheet[j][i] = tga_get_image_data_u8_ptr(t)[j * width + i];
	}
    }

  for (guint i = 0; i < targa_get_color_map_length (t) - first; i ++)
    bg.bg[id].map.palette[i] = tga_get_color_map_data_u16_ptr(t)[i + first];
}

void bg_set_bmp8_from_tga (int id, targa_image_t *t)
{
  guint width, height;
  int first = targa_get_color_map_first_index (t);

  targa_get_image_dimensions (t, &width, &height);
  bg.bg[id].bmp8.width = width;
  bg.bg[id].bmp8.height = height;
  for (guint j = 0; j < height; j ++)
    {
      for (guint i = 0; i < width; i ++)
	{
	  bg.bg[id].bmp8.bmp[j][i] = tga_get_image_data_u8_ptr(t)[j * width + i];
	}
    }

  for (guint i = 0; i < targa_get_color_map_length (t) - first; i ++)
    bg.bg[id].bmp8.palette[i] = tga_get_color_map_data_u16_ptr(t)[i + first];
}

void bg_set_bmp8_from_resource (int id, const gchar *resource)
{
  targa_image_t *t = tga_load_from_resource (resource);
  bg_set_bmp8_from_tga (id, t);
  tga_free (t);
}  

void bg_set_bmp16_from_tga (int id, targa_image_t *t)
{
  guint width, height;
  targa_get_image_dimensions (t, &width, &height);
  bg.bg[id].bmp16.width = width;
  bg.bg[id].bmp16.height = height;
  bg.bg[id].type = BG_TYPE_BMP16;
  for (guint j = 0; j < height; j ++)
    {
      for (guint i = 0; i < width ; i ++)
	{
	  bg.bg[id].bmp16.bmp[j][i] = tga_get_image_data_u16_ptr(t)[j * width + i];
	}
    }
}

void bg_set_bmp16_from_resource (int id, const gchar *resource)
{
  targa_image_t *t = tga_load_from_resource (resource);
  bg_set_bmp16_from_tga (id, t);
  tga_free (t);
}  


cairo_surface_t *
bg_render_to_cairo_surface (int id)
{
  switch (bg.bg[id].type)
    {
    case BG_TYPE_MAP:
      return bg_render_map_to_cairo_surface (id);
      break;
    case BG_TYPE_BMP8:
      return bg_render_bmp8_to_cairo_surface (id);
      break;
    case BG_TYPE_BMP16:
      return bg_render_bmp16_to_cairo_surface (id);
      break;
    }
g_return_val_if_reached (NULL);
}

static cairo_surface_t *
bg_render_map_to_cairo_surface (int id)
{
    cairo_surface_t *surf;
    guint32 *data;
    int stride;
    int tile_j, tile_i, delta_tile_i, delta_tile_j;
    int map_index;
    guint32 index, c;
    guint width, height;

    width = bg.bg[id].map.width;
    height = bg.bg[id].map.height;
    surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width * TILE_WIDTH, height * TILE_HEIGHT);
    data = xcairo_image_surface_get_argb32_data (surf);
    stride = xcairo_image_surface_get_argb32_stride (surf);
    xcairo_surface_flush (surf);

    for (guint map_j = 0; map_j < height; map_j ++)
    {
        for (guint map_i = 0; map_i < width; map_i ++)
        {
            /* Fill in the tile brush */
            map_index = bg.bg[id].map.map[map_j][map_i];
            delta_tile_j = (map_index / TILESHEET_WIDTH_IN_TILES) * TILE_HEIGHT;
            delta_tile_i = (map_index % TILESHEET_WIDTH_IN_TILES) * TILE_WIDTH;
            for (tile_j = 0; tile_j < TILE_HEIGHT; tile_j ++)
            {
                for (tile_i = 0; tile_i < TILE_WIDTH; tile_i ++)
                {
                    index = bg.bg[id].map.tilesheet[delta_tile_j + tile_j][delta_tile_i + tile_i];

		    c = adjust_colorval (bg.bg[id].map.palette[index]);
		    data[(map_j * TILE_HEIGHT + tile_j) * stride + (map_i * TILE_WIDTH + tile_i)] = c;
                }
            }
        }
    }
    xcairo_surface_mark_dirty (surf);
    return surf;
}

static cairo_surface_t *
bg_render_bmp8_to_cairo_surface (int id)
{
  guint width, height, stride;
  guint32 *data;
  guint16 c16;
  guint8 index;
  cairo_surface_t *surf;
  width = bg.bg[id].bmp8.width;
  height = bg.bg[id].bmp8.height;

  surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
  data = xcairo_image_surface_get_argb32_data (surf);
  stride = xcairo_image_surface_get_argb32_stride (surf);
  xcairo_surface_flush (surf);
  for (guint j = 0; j < height; j++)
    {
      for (guint i = 0; i < width; i++)
        {
	  index = bg.bg[id].bmp8.bmp[j][i];
	  c16 = bg.bg[id].bmp8.palette[index];
	  data[j * stride + i] = adjust_colorval (c16);
        }
    }
    xcairo_surface_mark_dirty (surf);
  return surf;
}

static cairo_surface_t *
bg_render_bmp16_to_cairo_surface (int id)
{
  guint width, height, stride;
  guint32 *data;
  guint16 c16;
  cairo_surface_t *surf;
  width = bg.bg[id].bmp16.width;
  height = bg.bg[id].bmp16.height;

  surf = xcairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
  data = xcairo_image_surface_get_argb32_data (surf);
  stride = xcairo_image_surface_get_argb32_stride (surf);
  xcairo_surface_flush (surf);
  for (guint j = 0; j < height; j++)
    {
      for (guint i = 0; i < width; i++)
        {
	  c16 = bg.bg[id].bmp16.bmp[j][i];
	  data[j * stride + i] = adjust_colorval (c16);
        }
    }
    xcairo_surface_mark_dirty (surf);
  return surf;
}

void bg_get_transform (int id, double *scroll_x, double *scroll_y, double *rotation_center_x,
		       double *rotation_center_y, double *rotation, double *expansion)
{
  *scroll_x = bg.bg[id].scroll_x;
  *scroll_y = bg.bg[id].scroll_y;
  *rotation_center_x = bg.bg[id].rotation_center_x;
  *rotation_center_y = bg.bg[id].rotation_center_y;
  *rotation = bg.bg[id].rotation;
  *expansion = bg.bg[id].expansion;
}
