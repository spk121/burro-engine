#ifndef BURRO_OBJ_H
#define BURRO_OBJ_H

#include "../x/xguile.h"

enum obj_const_tag
  {
    SPRITESHEET_HEIGHT = 1024,
    SPRITESHEET_WIDTH = 1024,
    MAIN_OBJ_COUNT = 4096,
    SUB_OBJ_COUNT = 4096,
    OBJ_COUNT = MAIN_OBJ_COUNT + SUB_OBJ_COUNT,
    SPRITESHEET_COUNT = 2
  };


void obj_hide (int id);
void obj_show (int id);
bool obj_is_shown (int id);
void obj_init (int id, int spritesheet_i, int spritesheet_j, int sprite_width, int sprite_height,
		 double rotation_center_x, double rotation_center_y, bool hflip, bool vflip);
void obj_set_spritesheet_origin (int id, int spritesheet_i, int spritesheet_j);

void obj_set (int id, int priority, double x, double y, double rotation, double expansion);
void obj_set_priority (int id, int priority);
int obj_get_priority (int id);
void obj_set_rotation_expansion (int id, double rotation, double expansion);
void obj_set_position (int id, double x, double y);
void obj_set_palette_offset (int id, int offset);
void obj_get_location (int id, double *x, double *y, double *rotation_center_x, double *rotation_center_y,
			 double *rotation, double *expansion);

void obj_set_spritesheet_from_file (int tilesheet_id, const char *filename);
cairo_surface_t *obj_render_to_cairo_surface (int id);

SCM G_obj_hide (SCM gid);
SCM G_obj_show (SCM gid);

#endif
