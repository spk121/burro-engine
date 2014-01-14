#ifndef BURRO_OBJ_H
#define BURRO_OBJ_H

enum obj_const_tag
  {
    OBJSHEET_HEIGHT = 256,
    OBJSHEET_WIDTH = 256,
    OBJSHEET_PALETTE_COLORS_COUNT = 512,
    MAIN_OBJ_COUNT = 128,
    SUB_OBJ_COUNT = 128,
  };


void obj_hide (int id);
void obj_show (int id);
gboolean obj_is_shown (int id);
void obj_init (int id, int spritesheet_i, int spritesheet_j, int sprite_width, int sprite_height,
		 double rotation_center_x, double rotation_center_y, gboolean hflip, gboolean vflip,
		 int palette_offset);
void obj_set_spritesheet_origin (int id, int spritesheet_i, int spritesheet_j);

void obj_set (int id, int priority, double x, double y, double rotation, double expansion,
		int palette_offset);
void obj_set_priority (int id, int priority);
int obj_get_priority (int id);
void obj_set_rotation_expansion (int id, double rotation, double expansion);
void obj_set_position (int id, double x, double y);
void obj_set_palette_offset (int id, int offset);
void obj_get_location (int id, double *x, double *y, double *rotation_center_x, double *rotation_center_y,
			 double *rotation, double *expansion);

void obj_set_tilesheet_from_resource (int sub_flag, const char *resource);
cairo_surface_t *obj_render_to_cairo_surface (int id);

#endif
