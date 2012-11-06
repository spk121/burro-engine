#pragma once

/** \file engine.h
	\brief The public API of the Burro 2D Game Engine

*/

#include <stdint.h>
#include <stdbool.h>
#include "engine_constants.h"
#include "engine_priv.h"

#define BASE_WIDTH 240
#define BASE_HEIGHT 192

/* The engine has 2 different displays: Main and Sub */
#define MAIN_SCREEN_WIDTH_IN_PIXELS BASE_WIDTH
#define MAIN_SCREEN_HEIGHT_IN_PIXELS BASE_HEIGHT
#define SUB_SCREEN_WIDTH_IN_PIXELS BASE_WIDTH
#define SUB_SCREEN_HEIGHT_IN_PIXELS BASE_HEIGHT

/* Since the pixels in the screen are tiny compared to a normal screen,
   they are magnified */
#define MAIN_SCREEN_MAGNIFICATION 2
#define SUB_SCREEN_MAGNIFICATION 1

/* Colors are stored as 4-byte RGBA values in 256 color palettes */
#define PALETTE_COLORS_COUNT 256
#define BYTES_PER_COLOR 4

/* There are four different z-levels */
#define PRIORITY_COUNT (4)

/* 3 different types of backgrounds
   - tile/map: a 2D array of indices to square tiles
   - bmp8: a bitmap where each byte is an index to a 256 color palette
   - bmp32: a bitmap where each quad is an RGBA colorval
*/

#define MAIN_BACKGROUNDS_COUNT 4
#define SUB_BACKGROUNDS_COUNT 2
#define MAP_HEIGHT_IN_TILES 128
#define MAP_WIDTH_IN_TILES 128
#define TILE_HEIGHT_IN_PIXELS 8
#define TILE_WIDTH_IN_PIXELS 8
#define BMP8_WIDTH_IN_PIXELS BASE_WIDTH
#define BMP8_HEIGHT_IN_PIXELS BASE_HEIGHT
#define BMP32_WIDTH_IN_PIXELS BASE_WIDTH
#define BMP32_HEIGHT_IN_PIXELS BASE_HEIGHT

/* Foregrounds are sprites that have associated affine transforms */
#define MAIN_SPRITES_COUNT (128)
#define SUB_SPRITES_COUNT (128)

/* Tiles and sprites are packed into a 2D bmp sprite sheets */
#define TILESHEET_HEIGHT_IN_TILES (32)
#define TILESHEET_WIDTH_IN_TILES (32)
#define TILESHEET_HEIGHT_IN_PIXELS ((TILESHEET_HEIGHT_IN_TILES) * (TILE_HEIGHT_IN_PIXELS))
#define TILESHEET_WIDTH_IN_PIXELS ((TILESHEET_WIDTH_IN_TILES) * (TILE_HEIGHT_IN_PIXELS))
#define OBJSHEET_HEIGHT_IN_TILES (32)
#define OBJSHEET_WIDTH_IN_TILES (32)
#define OBJSHEET_HEIGHT_IN_PIXELS ((OBJSHEET_HEIGHT_IN_TILES) * (TILE_HEIGHT_IN_PIXELS))
#define OBJSHEET_WIDTH_IN_PIXELS ((OBJSHEET_WIDTH_IN_TILES) * (TILE_WIDTH_IN_PIXELS))

/* The sound engine has 3 types of generators
   - square wave tone
   - noise
   - wav
*/


#define AUDIO_SAMPLE_RATE_IN_HZ (22050)
#define WAVEFORM_MAX_DURATION_IN_SEC (12)
#define WAVEFORM_MAX_DURATION_IN_SAMPLES ((WAVEFORM_MAX_DURATION_IN_SEC) * (AUDIO_SAMPLE_RATE_IN_HZ))

/* These are the main event timers */
#define TIMER_COUNT (4)

/* Macros for color values */
#define RGBA_TO_RED(x)    ((x) & 0x000000ff)
#define RGBA_TO_GREEN(x) (((x) & 0x0000ff00)>>8)
#define RGBA_TO_BLUE(x)  (((x) & 0x00ff0000)>>16)
#define RGBA_TO_ALPHA(x) (((x) & 0xff000000)>>24)
#define RGBA_TO_RED_RATIO(x)   ((float)(RGBA_TO_RED(x))/256.0f)
#define RGBA_TO_GREEN_RATIO(x) ((float)(RGBA_TO_GREEN(x))/256.0f)
#define RGBA_TO_BLUE_RATIO(x)  ((float)(RGBA_TO_BLUE(x))/256.0f)
#define RGBA_TO_ALPHA_RATIO(x) ((float)(RGBA_TO_ALPHA(x))/256.0f)
#define RGBA(r,g,b,a) ((r) | ((g)<<8) | ((b)<<16) | ((a)<<24))


struct bg_map_data
{
    int map_height_in_tiles;
    int map_width_in_tiles;
    uint16_t map[MAP_HEIGHT_IN_TILES][MAP_WIDTH_IN_TILES];
    uint8_t tiles[TILESHEET_HEIGHT_IN_PIXELS][TILESHEET_WIDTH_IN_PIXELS];
    uint32_t palette[PALETTE_COLORS_COUNT];
};

struct bg_bmp8_data
{
    int height_in_pixels;
    int width_in_pixels;
    uint8_t bmp[BMP8_HEIGHT_IN_PIXELS][BMP8_WIDTH_IN_PIXELS];
    uint32_t palette[PALETTE_COLORS_COUNT];
};

struct bg_bmp32_data
{
    int height_in_pixels;
    int width_in_pixels;
    uint32_t bmp[BMP32_HEIGHT_IN_PIXELS][BMP32_WIDTH_IN_PIXELS];
};

enum bg_entry_mode
{
    BG_MODE_UNKNOWN = 0,
    BG_MODE_MAP_AND_TILE,
    BG_MODE_INDEXED_BITMAP,
    BG_MODE_TRUE_COLOR_BITMAP
};

typedef struct bg_entry
{
    /* BG is display when true */
    _Bool enable;

    /** tile and map, palette bmp, or true color bmp */
    enum bg_entry_mode mode;

    /* z-level: 0 is foreground, 3 is background */
    int priority;

    /** the "user" or screen location of the rotation center of the background */
    double center_x, center_y;

    /** the "device" location of the rotation center of the background*/
    int center_i, center_j;

    /** the expansion factor of the background: 1.0 = 1 pixel per pixel */
    double expansion;

    /** the rotation angle of the background about its rotation center, in radians */
    double rotation;

    union
    {
        struct bg_map_data map;
        struct bg_bmp8_data bmp8;
        struct bg_bmp32_data bmp32;
    };
} bg_entry_t;

typedef struct obj_data
{
    uint8_t bmp[OBJSHEET_HEIGHT_IN_PIXELS][OBJSHEET_WIDTH_IN_PIXELS];
    uint32_t palette[PALETTE_COLORS_COUNT];
} obj_data_t;

typedef struct obj_entry
{
    /** Sprite is visible if true */
    _Bool enable;

    /** priority aka z-level. 0 to 3 where 0 is foreground */
    int priority;

    /** location of top-left corner of sprite in sprite sheet */
    int spritesheet_i, spritesheet_j;

    /** size of sprite in pixels */
    int sprite_width, sprite_height;

    /** the "user" or screen location of the rotation center of the sprite */
    double center_x, center_y;

    /** the "device" location of the rotation centere, aka, it bitmap row and column of its hotspot*/
    int center_i, center_j;

    /** the expansion factor of the sprite: 1.0 = 1 pixel per pixel */
    double expansion;

    /** the rotation angle of the sprite about its rotation center, in radians */
    double rotation;
} obj_entry_t;


/* Tones are squarish waves */
typedef struct tone_entry
{
    /** (Write Only) When Game sets this TRUE, this tone will start on the next idle cycle.
        Engine will set it FALSE once it has been processed. */
    _Bool start_trigger;

    /** (Write Only) When Game sets this TRUE, the currently playing tone will stop on the next idle cycle.
        Engine will set it FALSE once it has been processed. */
    _Bool stop_trigger;

    /** (Read) Engine sets this to TRUE when it is playing a tone on this channel.  */
    _Bool is_playing;

    /** (Write) the duration of the attack portion of the tone in seconds */
    double attack_duration;

    /** (Write) the duration of the decay portion of the tone in seconds */
    double decay_duration;

    /** (Write) the duration of the release portion of the tone in seconds */
    double release_duration;

    /** (Write) the duration of the sustain portion of the tone in seconds */
    double sustain_duration;

    /** (Write) the initial frequency of the tone in Hz */
    double initial_frequency;

    /** (Write) the frequency of the tone at the end of the attack, in Hz */
    double attack_frequency;

    /** (Write) the frequency of the tone at the end of the decay, in Hz */
    double sustain_frequency;

    /** (Write) the frequency of the tone at the end of the release, in Hz */
    double release_frequency;

    /** (Write) the amplitude of the tone at the end of the attack, from 0 to 1.  Usually 1 */
    double attack_amplitude;

    /** (Write) the amplitude of the tone at the end of the decay, from 0 to 1.  Usually between 0.5 and 1 */
    double sustain_amplitude;

    /** (Write) the ratio between the length of the high part of the square wave to the total period, usually 0.5 */
    double duty;
} tone_entry_t;

typedef struct noise_entry
{
    /** (Write Only) When Game sets this TRUE, this noise will start on the next idle cycle.
        Engine will set it FALSE once it has been processed. */
    _Bool start_trigger;

    /** (Write Only) When Game sets this TRUE, the currently playing noise will stop on the next idle cycle.
        Engine will set it FALSE once it has been processed. */
    _Bool stop_trigger;

    /** (Read) Engine sets this to TRUE when it is playing a noise on this channel.  */
    _Bool is_playing;

    /** (Write) the duration of the attack portion of the noise in seconds */
    double attack_duration;

    /** (Write) the duration of the decay portion of the noise in seconds */
    double decay_duration;

    /** (Write) the duration of the release portion of the noise in seconds */
    double release_duration;

    /** (Write) the duration of the sustain portion of the noise in seconds */
    double sustain_duration;

    /** (Write) the initial frequency of the noise in Hz */
    double initial_frequency;

    /** (Write) the frequency of the noise at the end of the attack, in Hz */
    double attack_frequency;

    /** (Write) the frequency of the noise at the end of the decay, in Hz */
    double sustain_frequency;

    /** (Write) the frequency of the noise at the end of the release, in Hz */
    double release_frequency;

    /** (Write) the amplitude of the noise at the end of the attack, from 0 to 1.  Usually 1 */
    double attack_amplitude;

    /** (Write) the amplitude of the noise at the end of the decay, from 0 to 1.  Usually between 0.5 and 1 */
    double sustain_amplitude;

    /** (Write) the ratio between the length of the high part of the square wave to the total period, usually 0.5 */
    double duty;
} noise_entry_t;

typedef struct wave_entry
{
    /** (Write Only) When Game sets this TRUE, this tone will start on the next idle cycle.
        Engine will set it FALSE once it has been processed. */
    _Bool start_trigger;

    /** (Write Only) When Game sets this TRUE, the currently playing tone will stop on the next idle cycle.
        Engine will set it FALSE once it has been processed. */
    _Bool stop_trigger;

    /** (Read) Engine sets this to TRUE when it is playing a tone on this channel.  */
    _Bool is_playing;

    /** (Write) Count of the number of samples in the waveform. */
    int count;

    /** (Write) An 8-bit unsigned PCM waveform sampled at WAVEFORM_SAMPLE_RATE_MAX_IN_HZ */
    uint8_t wave[WAVEFORM_MAX_DURATION_IN_SAMPLES];
} wave_entry_t;


typedef int (*eng_delta_t_handler)(double delta_t);
typedef int (*eng_id_handler)(int id);

typedef struct engine_tag
{
    /* RW: When true, all graphics are drawn just as the background color */
    _Bool blank;

    /* RW: When false, all colors are unpacked as RGBA. When true BGR. */
    _Bool color_swap;

    /* 0.0 is black, 1.0 is normal */
    double brightness;

    /* The RGBA color displayed below all backgrounds and sprites */
    uint32_t bg_color;

    struct priv_entry priv;

    bg_entry_t main_bg[MAIN_BACKGROUNDS_COUNT];
    obj_entry_t main_obj[MAIN_SPRITES_COUNT];
    obj_data_t main_objsheet;

    bg_entry_t sub_bg[SUB_BACKGROUNDS_COUNT];
    obj_entry_t sub_obj[SUB_SPRITES_COUNT];
    obj_data_t sub_objsheet;

    tone_entry_t tone[TONE_COUNT];
    wave_entry_t wave[WAVE_COUNT];
    noise_entry_t noise[NOISE_COUNT];

    //struct timer_entry timers[TIMER_COUNT];
    _Bool key_up;
    _Bool key_down;
    _Bool key_left;
    _Bool key_right;
    _Bool key_a;
    _Bool key_b;
    _Bool key_x;
    _Bool key_y;
    _Bool key_left_trigger;
    _Bool key_right_trigger;
    _Bool key_start;
    _Bool key_select;
    eng_delta_t_handler do_idle;
    eng_delta_t_handler do_after_draw_frame;
    //eng_callback_handler do_after_keypress;
    eng_id_handler do_after_timer[TIMER_COUNT];
    eng_id_handler do_sound_channel[CHANNEL_COUNT];
} engine_t;

extern engine_t e;

/** Called by the main to initialize the engine */
void engine_initialize (int *argc, char ***argv, char *title);

/** Called by the main to start the main loop */
void engine_loop (void);

/** Called by Game to request quitting the engine */
void engine_finalize (void);

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
