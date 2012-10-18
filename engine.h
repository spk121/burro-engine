#pragma once

/** \file engine.h
	\brief The public API of the Burro 2D Game Engine

*/

#include <stdint.h>
#include <stdbool.h>

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
#define BG_MODE_TILE_AND_MAP (1)
#define BG_MODE_INDEXED_BITMAP (2)
#define BG_MODE_TRUE_COLOR_BITMAP (3)

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
#define MAIN_TRANSFORMS_COUNT (32)
#define SUB_SPRITES_COUNT (128)
#define SUB_TRANSFORMS_COUNT (32)

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

#define TONE_COUNT (3)
#define WAVE_COUNT (2)
#define NOISE_COUNT (1)
#define WAVEFORM_SAMPLE_RATE_MAX_IN_HZ (22050)
#define WAVEFORM_MAX_DURATION_IN_SEC (12)
#define WAVEFORM_MAX_DURATION_IN_SAMPLES ((WAVEFORM_MAX_DURATION_IN_SEC) * (WAVEFORM_SAMPLE_RATE_MAX_IN_HZ))

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


struct bg_map_data
{
    uint16_t map_height_in_tiles;
    uint16_t map_width_in_tiles;
    uint16_t map[MAP_HEIGHT_IN_TILES][MAP_WIDTH_IN_TILES];
    uint8_t tiles[TILESHEET_HEIGHT_IN_PIXELS][TILESHEET_WIDTH_IN_PIXELS];
    uint32_t palette[PALETTE_COLORS_COUNT];
};

struct bg_bmp8_data
{
    uint16_t height_in_pixels;
    uint16_t width_in_pixels;
    uint8_t bmp[BMP8_HEIGHT_IN_PIXELS][BMP8_WIDTH_IN_PIXELS];
    uint32_t palette[PALETTE_COLORS_COUNT];
};

struct bg_bmp32_data
{
    uint16_t height_in_pixels;
    uint16_t width_in_pixels;
    uint32_t bmp[BMP32_HEIGHT_IN_PIXELS][BMP32_WIDTH_IN_PIXELS];
};

struct bg_entry
{
    /* BG is display when true */
    bool enable;

    /** tile and map, palette bmp, or true color bmp */
    uint8_t mode;

    /* z-level: 0 is foreground, 3 is background */
    uint8_t priority;

    /* Affine transform */
    float xx, xy, yx, yy, x0, y0;

    union
    {
        struct bg_map_data map;
        struct bg_bmp8_data bmp8;
        struct bg_bmp32_data bmp32;
    };
};

struct obj_data
{
    uint8_t bmp[OBJSHEET_HEIGHT_IN_PIXELS][OBJSHEET_WIDTH_IN_PIXELS];
    uint32_t palette[PALETTE_COLORS_COUNT];
};

struct obj_entry
{
    /** Sprite is visible if true */
    bool enable;

    /** x location of the sprite in pixels */
    uint16_t x;

    /** y location of the sprite in pixels */
    uint16_t y;

    /** priority aka z-level. 0 to 3 where 0 is foreground */
    uint8_t priority;

    /** index of an affine transform to be applied */
    uint8_t affine_index;
};

struct trans_entry
{
    float xx, xy, yx, yy, x0, y0;
};

/* Tones are square waves */
struct tone_entry
{
    /** frequency of the tone in cycles/sec */
    float freq;

    /** amplitude of the tone from 0.0 to 1.0 */
    float amplitude;

    /** duration of the tone in sec */
    float duration;

    /** ratio between high and low in the square wave, from 0.0 to 1.0 */
    float duty;

    /** rate of change in frequency of tone in cycles / sec^2 */
    float sweep_delta_freq;

    /** duration of the sweep in sec */
    float sweep_duration;

    /** rate of change of amplitude in units / sec */
    float env_delta_amplitude;

    /** duration of the rate of change of the tone */
    float env_duration;
};

/* White noise is generated by creating a squareish waveform where
   each sample is randomly either high or low. */
struct noise_entry
{
    /** frequency of the tone in cycles/sec */
    float freq;

    /** amplitude of the tone from 0.0 to 1.0 */
    float amplitude;

    /** duration of the tone in sec */
    float duration;

    /** ratio between high and low in the square wave, from 0.0 to 1.0 */
    float duty;

    /** rate of change in frequency of tone in cycles / sec^2 */
    float sweep_delta_freq;

    /** duration of the sweep in sec */
    float sweep_duration;

    /** rate of change of amplitude in units / sec */
    float env_delta_amplitude;

    /** duration of the rate of change of the tone */
    float env_duration;
};

struct wave_entry
{
    uint8_t wave[WAVEFORM_MAX_DURATION_IN_SAMPLES];
};

typedef struct eng engine_t;

typedef int (*eng_callback_handler)(engine_t *e, double delta_t);
typedef int (*eng_timer_handler)(engine_t *e, int state);

struct eng
{
    /* RW: When true, all graphics are drawn just as the background color */
    bool blank;

    /* RW: When false, all colors are unpacked as RGBA. When true BGR. */
    bool color_swap;

    /* 0.0 is black, 1.0 is normal */
    double brightness;

    /* The RGBA color displayed below all backgrounds and sprites */
    uint32_t bg_color;

    struct bg_entry main_bg[MAIN_BACKGROUNDS_COUNT];
    struct obj_entry main_obj[MAIN_SPRITES_COUNT];
    struct obj_data main_objsheet;
    struct trans_entry main_trans[MAIN_TRANSFORMS_COUNT];

    struct bg_entry sub_bg[SUB_BACKGROUNDS_COUNT];
    struct obj_entry sub_obj[SUB_SPRITES_COUNT];
    struct obj_data sub_objsheet;
    struct trans_entry sub_trans[SUB_TRANSFORMS_COUNT];

    struct tone_entry tone[TONE_COUNT];
    struct wave_entry wave[WAVE_COUNT];
    struct noise_entry noise[NOISE_COUNT];

    //struct timer_entry timers[TIMER_COUNT];
    uint8_t key_up;
    uint8_t key_down;
    uint8_t key_left;
    uint8_t key_right;
    uint8_t key_a;
    uint8_t key_b;
    uint8_t key_left_trigger;
    uint8_t key_right_trigger;
    uint8_t key_start;
    uint8_t key_select;
    eng_callback_handler do_idle;
    eng_callback_handler do_before_draw_frame;
    eng_callback_handler do_after_keypress;
    eng_timer_handler do_after_timer[TIMER_COUNT];

};

extern engine_t e;

void eng_init (void);
void eng_main (void);

/*
  Local Variables:
  mode:C
  c-file-style:"linux"
  tab-width:4
  c-basic-offset: 4
  indent-tabs-mode:nil
  End:
*/
