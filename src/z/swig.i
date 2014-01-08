/* SWIG interface for game engine */
%module burro
%include "glib-types.i"
%{
#include "../y/bg.h"
%}

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
gboolean bg_is_shown (int id);
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
void bg_set_bmp8_from_resource (int id, const gchar *resource);
void bg_set_bmp16_from_tga (int id, targa_image_t *t);
void bg_set_bmp16_from_resource (int id, const gchar *resource);
void bg_get_transform (int id, double *scroll_x, double *scroll_y, double *rotation_center_x,
		       double *rotation_center_y, double *rotation, double *expansion);

