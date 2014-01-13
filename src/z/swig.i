/* SWIG interface for game engine */
%module burro
%include "glib-types.i"
%{
#include "../y/bg.h"
#include "../y/obj.h"
#include "../y/loop.h"
#include "../y/pulseaudio.h"
%}

enum bg_const_tag
  {
    MAIN_BACKGROUNDS_COUNT = 4,
    SUB_BACKGROUNDS_COUNT = 4,
    MAP_HEIGHT = 512,
    MAP_WIDTH = 512,
    TILESHEET_HEIGHT = 256,
    TILESHEET_WIDTH = 256,
    TILE_HEIGHT = 8,
    TILE_WIDTH = 8,
    TILESHEET_HEIGHT_IN_TILES = (256/8),
    TILESHEET_WIDTH_IN_TILES = (256/8),
    PALETTE_COLORS_COUNT = 256,
    BMP8_HEIGHT = 512,
    BMP8_WIDTH = 512,
    BMP16_HEIGHT = 512,
    BMP16_WIDTH = 512,
    BG_COLOR16_BLACK = 0x0,
  };

enum bg_index_tag
  {
    BG_MAIN_0 = 0,
    BG_MAIN_1 = 1,
    BG_MAIN_2 = 2,
    BG_MAIN_3 = 3,
    BG_SUB_0 = 4,
    BG_SUB_1 = 5,
    BG_SUB_2 = 6,
    BG_SUB_3 = 7
  };

typedef enum bg_index_tag bg_index_t;

enum bg_type_tag
  {
    BG_TYPE_MAP,
    BG_TYPE_BMP8,
    BG_TYPE_BMP16,
  };

typedef enum bg_type_tag bg_type_t;

void bg_set_backdrop_color (guint16 c16);
void bg_get_backdrop_color_rgb (double *r, double *g, double *b);

int bg_get_priority (int id);
//gboolean bg_is_shown (int id);
void bg_hide (int id);
void bg_init (int id, bg_type_t type, guint width, guint height);
void bg_rotate (int id, double angle);
void bg_scroll (int id, double dx, double dy);
void bg_set (int id, double rotation, double expansion, double scroll_x, double scroll_y,
	     double rotation_center_x, double rotation_center_y);
void bg_set_rotation_center (int id, double rotation_center_x, double rotation_center_y);
void bg_set_priority (int id, int priority);
void bg_set_rotation (int id, double rotation);
void bg_set_rotation_expansion (int id, double rotation, double expansion);
void bg_set_expansion (int id, double expansion);
void bg_show (int id);
void bg_set_map_from_tga (int id, targa_image_t *t);
void bg_set_tilesheet_from_tga (int id, targa_image_t *t);
void bg_set_bmp8_from_tga (int id, targa_image_t *t);
void bg_set_bmp8_from_resource (int id, const char *resource);
void bg_set_bmp16_from_tga (int id, targa_image_t *t);
void bg_set_bmp16_from_resource (int id, const char *resource);
void bg_get_transform (int id, double *scroll_x, double *scroll_y, double *rotation_center_x,
		       double *rotation_center_y, double *rotation, double *expansion);

void obj_hide (int id);
void obj_show (int id);
// boolean obj_is_shown (int id);
void obj_init (int id, int spritesheet_i, int spritesheet_j, int sprite_width, int sprite_height,
               double rotation_center_x, double rotation_center_y, int hflip, int vflip,
               int palette_offset);
void obj_set_spritesheet_origin (int id, int spritesheet_i, int spritesheet_j);

void obj_set (int id, int priority, double x, double y, double rotation, double expansion,
              int palette_offset);
// void obj_set_priority (int id, int priority);
int obj_get_priority (int id);
void obj_set_rotation_expansion (int id, double rotation, double expansion);
void obj_set_position (int id, double x, double y);
void obj_set_palette_offset (int id, int offset);

// void obj_set_tilesheet_from_resource (int sub_flag, const char *resource);

void tone(int channel, double start_time,
          double D_attack, double D_decay, double D_sustain, double D_release,
          double F_initial, double F_attack, double F_sustain, double F_release,
          double A_attack, double A_sustain,
          double duty, int noise, int waveform);

////////////////////////////////////////////////////////////////
// loop.h

double loop_time(void);
