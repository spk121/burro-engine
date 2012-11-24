#include <string.h>
#include "engine.h"
#include "tga.h"
#include "cp437.h"

#define FONT_FILENAME "cga8.tga"
#define NULL_CANCELLABLE (0)

static void clear_bg0(void);
static void do_key_a(void);
static void do_key_down(void);
static void do_key_left(void);
static void do_key_right(void);
static void do_key_up(void);
void draw_gui(void);
int idle_cb(double delta_t);
void iprint_main(int bg, int i, int j, const char *str);
void load_bg0_art(void);
void play_noise(void) {}
void play_tone(void) {}

/* Game State */
_Bool key_up_prev = FALSE;
_Bool key_down_prev = FALSE;
_Bool key_left_prev = FALSE;
_Bool key_right_prev = FALSE;
_Bool key_a_prev = FALSE;
_Bool key_b_prev = FALSE;
_Bool splash_flag = TRUE;
_Bool initialized_flag = FALSE;
int control_index = 0;

typedef struct tune_tag
{
    double time;
    int channel;
    double frequency;
    double duration;
    int percussion;
} note_t;

#define TUNE_LEN (4083)
note_t tune[TUNE_LEN] = 
{
#include "tune.h"
};

#define CONTROL_COUNT (11)
#define TWELFTH_ROOT_OF_2 (1.059463094)
#define FORTY_EIGHTH_ROOT_OF_2 (1.014545335)
typedef struct note_control_tag
{
	char name[12];
	_Bool log;
	double value, minimum, maximum, step;
} note_control_t;

note_control_t control[CONTROL_COUNT] = {
	{"D_attack",  FALSE, 0.1, 0.0, 1.0, 0.05},
	{"D_decay",   FALSE, 0.1, 0.0, 1.0, 0.05},
	{"D_sustain", FALSE, 0.1, 0.0, 1.0, 0.05},
	{"D_release", FALSE, 0.1, 0.0, 1.0, 0.05},
	{"F_initial", TRUE,  440.0, 20.0, 12000.0, TWELFTH_ROOT_OF_2},
	{"F_attack",  TRUE,  440.0, 20.0, 12000.0, TWELFTH_ROOT_OF_2},
	{"F_sustain", TRUE,  440.0, 20.0, 12000.0, TWELFTH_ROOT_OF_2},
	{"F_release", TRUE,  440.0, 20.0, 12000.0, TWELFTH_ROOT_OF_2},
	{"A_attack",  FALSE, 1.0, 0.05, 1.0, 0.05},
	{"A_sustain", FALSE, 1.0, 0.05, 1.0, 0.05},
	{"Duty",      FALSE, 0.5, 0.0, 1.0, 0.05}};

void log_handler(const gchar *log_domain,
                    GLogLevelFlags log_level,
                    const gchar *message,
                    gpointer user_data)
{
    fprintf(stdout, "%s\n", message);
    fflush(stdout);
}

int main(int argc, char **argv)
{
	/* Prep the game engine */
    g_log_set_handler (NULL, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION, log_handler, NULL);
    g_log_set_handler ("GLib", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION, log_handler, NULL);
    g_log_set_handler ("Gtk", G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION, log_handler, NULL);

	engine_initialize(&argc, &argv, "tunes");
	g_type_init();

	e.do_idle = idle_cb;
	e.priv.run_full_speed_flag = TRUE;

	load_bg0_art();

	engine_loop();
	return 0;
}

static void clear_bg0()
{
	int u, v;
	for (v = 0; v < e.main_bg[0].map.map_height_in_tiles; v++)
	{
		for (u = 0; u < e.main_bg[0].map.map_width_in_tiles; u++)
		{
			e.main_bg[0].map.map[v][u] = 0;
		}
	}
}

static void do_key_a()
{
#if 0
	e.tone[0].attack_duration = control[0].value;
	e.tone[0].decay_duration = control[1].value;
	e.tone[0].sustain_duration = control[2].value;
	e.tone[0].release_duration = control[3].value;
	e.tone[0].initial_frequency = control[4].value;
	e.tone[0].attack_frequency = control[5].value;
	e.tone[0].sustain_frequency = control[6].value;
	e.tone[0].release_frequency = control[7].value;
	e.tone[0].attack_amplitude = control[8].value;
	e.tone[0].sustain_amplitude = control[9].value;
	e.tone[0].duty = control[10].value;
	e.tone[0].start_trigger = TRUE;
#endif
}

static void do_key_b()
{
	e.noise[0].attack_duration = control[0].value;
	e.noise[0].decay_duration = control[1].value;
	e.noise[0].sustain_duration = control[2].value;
	e.noise[0].release_duration = control[3].value;
	e.noise[0].initial_frequency = control[4].value;
	e.noise[0].attack_frequency = control[5].value;
	e.noise[0].sustain_frequency = control[6].value;
	e.noise[0].release_frequency = control[7].value;
	e.noise[0].attack_amplitude = control[8].value;
	e.noise[0].sustain_amplitude = control[9].value;
	e.noise[0].duty = control[10].value;
	e.noise[0].start_trigger = TRUE;
}

static void do_key_down()
{
	control_index ++;
	if(control_index >= CONTROL_COUNT)
		control_index = 0;
}

static void do_key_left()
{
	if(control[control_index].log)
		control[control_index].value /= control[control_index].step;
	else
		control[control_index].value -= control[control_index].step;
	if(control[control_index].value < control[control_index].minimum)
		control[control_index].value = control[control_index].minimum;
}

static void do_key_right()
{
	if(control[control_index].log)
		control[control_index].value *= control[control_index].step;
	else
		control[control_index].value += control[control_index].step;
	if(control[control_index].value > control[control_index].maximum)
		control[control_index].value = control[control_index].maximum;
}

static void do_key_up()
{
	control_index --;
	if(control_index < 0)
		control_index = CONTROL_COUNT - 1;
}

void draw_splash()
{
	char *str;
	if (initialized_flag == FALSE)
	{
		clear_bg0();
		iprint_main(0, 0, 0, "Midi Tune Tester");
		str = g_strdup_printf("Press %c %c",
                              CP437_UPWARDS_ARROW,
                              CP437_DOWNWARDS_ARROW);
		iprint_main(0, 0, 2, str);
		g_free(str);
		iprint_main(0, 0, 3, "to switch tunes");
		iprint_main(0, 0, 5, "Press 'A' to play it");
	}
}

void draw_gui()
{
	int i;
	char *str;
	const int start_row = 3;
	for(i = 0; i < CONTROL_COUNT; i++)
	{
		str = g_strdup_printf("%c %12s %8.2f",
				      ((i == control_index) 
				       ? CP437_BLACK_RIGHT_POINTING_POINTER 
				       : CP437_SPACE),
				      control[i].name,
				      control[i].value);
		iprint_main(0, 0, start_row + i, str);
		g_free(str);
	}
	if (e.tone[0].is_playing)
		e.main_bg[0].map.map[5][start_row + CONTROL_COUNT + 1] = CP437_BEAMED_EIGHTH_NOTES;
	else
		e.main_bg[0].map.map[5][start_row + CONTROL_COUNT + 1] = CP437_SPACE;
	if (e.noise[0].is_playing)
		e.main_bg[0].map.map[7][start_row + CONTROL_COUNT + 1] = CP437_DOUBLE_EXCLAMATION_POINT;
	else
		e.main_bg[0].map.map[7][start_row + CONTROL_COUNT + 1] = CP437_SPACE;
}

int idle_cb(double delta_t)
{
    static double t = 0.0;
    static int tune_index = 0;
    int c;
    double sustain;

    if (splash_flag == TRUE && (e.key_start || e.key_a || e.key_b))
	{
		splash_flag = FALSE;
		clear_bg0();
	}

	if (splash_flag)
	{
		draw_splash();
		return TRUE;
	}

    t += delta_t;
    // g_debug("T %f delta_t %f", t, delta_t);
    while(tune_index < TUNE_LEN && tune[tune_index].time < t)
    {
        if(tune[tune_index].percussion)
        {
            e.noise[0].attack_duration = 0.01;
            e.noise[0].decay_duration = 0.01;
            e.noise[0].sustain_duration = 0.01;
            e.noise[0].release_duration = 0.05;
            e.noise[0].initial_frequency = 8000;
            e.noise[0].attack_frequency = 6000;
            e.noise[0].sustain_frequency = tune[tune_index].frequency;
            e.noise[0].release_frequency = 100;
            e.noise[0].attack_amplitude = 0.5;
            e.noise[0].sustain_amplitude = 0.1;
            e.noise[0].duty = 0.5;
            e.noise[0].start_trigger = TRUE;
        }
        else
        {
            c = tune[tune_index].channel;
            sustain = tune[tune_index].duration - 0.2;
            if (sustain < 0.0)
                sustain = 0.0;
            e.tone[c].attack_duration = 0.0;
            e.tone[c].decay_duration = 0.1;
            e.tone[c].sustain_duration = sustain;
            e.tone[c].release_duration = 0.2;
            e.tone[c].initial_frequency = tune[tune_index].frequency;
            e.tone[c].attack_frequency = tune[tune_index].frequency;
            e.tone[c].sustain_frequency = tune[tune_index].frequency;
            e.tone[c].release_frequency = tune[tune_index].frequency;
            e.tone[c].attack_amplitude = 0.5;
            e.tone[c].sustain_amplitude = 0.2;
            e.tone[c].duty = 0.5;
            e.tone[c].start_trigger = TRUE;
        }
        tune_index ++;
    }
        

	if (key_up_prev == FALSE && e.key_up == TRUE)
		do_key_up();
	if (key_down_prev == FALSE && e.key_down == TRUE)
		do_key_down();
	if (key_left_prev == FALSE && e.key_left == TRUE)
		do_key_left();
	if (key_right_prev == FALSE && e.key_right == TRUE)
		do_key_right();

	draw_gui();

	if (key_a_prev == FALSE && e.key_a == TRUE)
		do_key_a();
	if (key_b_prev == FALSE && e.key_b == TRUE)
		do_key_b();

	key_up_prev = e.key_up;
	key_down_prev = e.key_down;
	key_left_prev = e.key_left;
	key_right_prev = e.key_right;
	key_a_prev = e.key_a;
	key_b_prev = e.key_b;
}

void iprint_main(int bg, int i, int j, const char *str)
{
	int c, len;
	int u, v;
	u = i;
	v = j;
	len = strlen(str);
	for (c = 0; c < len; c ++)
	{
		if (u > e.main_bg[bg].map.map_width_in_tiles)
		{
			u -= e.main_bg[bg].map.map_width_in_tiles;
			v ++;
		}
		if (v < e.main_bg[bg].map.map_height_in_tiles)
			e.main_bg[bg].map.map[u][v] = str[c];
		u ++;
	}
}
	
void load_bg0_art()
{
	GFile *font_file;
	GFileInputStream *font_stream;
	GError *font_err = NULL;
	targa_error_t tga_err;
	targa_image_t tga;

	font_file = g_file_new_for_path(FONT_FILENAME);
	font_stream = g_file_read(font_file, NULL_CANCELLABLE, &font_err);
	tga_err = targa_parse_stream(G_INPUT_STREAM(font_stream), &tga); 
	g_object_unref (font_file);
	

	e.main_bg[0].enable = TRUE;
	e.main_bg[0].mode = BG_MODE_MAP_AND_TILE;
	e.main_bg[0].priority = 0;
	e.main_bg[0].map.map_height_in_tiles = 30;
	e.main_bg[0].map.map_width_in_tiles = 40;
	memcpy(e.main_bg[0].map.tiles, tga.data.image_data, 32768);
	e.main_bg[0].map.palette[0] = 0xff112233;
	e.main_bg[0].map.palette[1] = 0xff00eeee;
}

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
