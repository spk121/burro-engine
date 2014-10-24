/* eng.cpp -- Top-level graphics engine

   Copyright 2014, Michael L. Gran

   This file is part of the Project Burro game engine.

   Project Burro is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   Project Burro is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Project Burro.  If not, see
   <http://www.gnu.org/licenses/>. */

#include <stdbool.h>
#include <cairo.h>
#include <SDL.h>
#include "eng.hpp"
#include "draw.hpp"
#include "const.h"
#include "Interpreter.hpp"
#include "js_func.hpp"

static bool blank_flag = false;
static bool colorswap_flag = false;
static double brightness = 1.0;

static SDL_Window *window = NULL;
static SDL_Renderer *main_screen = NULL;

bool
eng_is_blank ()
{
  return blank_flag;
}

void
eng_blank ()
{
  blank_flag = true;
}

void
eng_unblank ()
{
  blank_flag = false;
}

bool 
eng_is_colorswap ()
{
  return colorswap_flag;
}

void
eng_colorswap ()
{
  colorswap_flag = true;
}

void
eng_uncolorswap ()
{
  colorswap_flag = false;
}

double
eng_get_brightness ()
{
  return brightness;
}

void
eng_set_brightness (double b)
{
  brightness = b;
}

void eng_initialize ()
{
  // Create an application window with the following settings:
  window = SDL_CreateWindow("Project Burro",
			    SDL_WINDOWPOS_UNDEFINED,
			    SDL_WINDOWPOS_UNDEFINED,
			    MAIN_SCREEN_WIDTH * MAIN_SCREEN_MAGNIFICATION,
			    MAIN_SCREEN_HEIGHT * MAIN_SCREEN_MAGNIFICATION,
			    SDL_WINDOW_SHOWN
			    );
  
  main_screen = SDL_CreateRenderer(window, -1, 0);
}

void eng_finalize()
{
  SDL_DestroyRenderer(main_screen);
  SDL_DestroyWindow(window);
}

void eng_present(void)
{
    SDL_Texture *cr;
    uint8_t *pixels, *pos;
    int pitch;
    
    unsigned char *data;
    int width, height, stride;
    cairo_surface_t *surface;
    
    /* Have the video view draw the video model onto the screen */
    cr = SDL_CreateTexture(main_screen,
                           SDL_PIXELFORMAT_RGB888,
                           SDL_TEXTUREACCESS_STREAMING,
                           MAIN_SCREEN_WIDTH,
                           MAIN_SCREEN_HEIGHT);
    
    surface = draw_get_main_screen_surface ();
    data = cairo_image_surface_get_data (surface);
    width = cairo_image_surface_get_width (surface);
    height = cairo_image_surface_get_height (surface);
    stride = cairo_image_surface_get_stride (surface);
    
    SDL_LockTexture(cr, NULL, (void **)(&pixels), &pitch);
    for (int j = 0; j < height; j ++) {
        for (int i = 0; i < width; i ++) {
            pos = pixels + j * pitch + 4 * i;
            *((uint32_t *) pos) = *((uint32_t *) (data + j * stride + 4 * i));
        }
    }
    SDL_UnlockTexture(cr);
    
    SDL_RenderClear(main_screen);
    SDL_RenderCopy(main_screen, cr, NULL, NULL);
    SDL_RenderPresent(main_screen);
    
    SDL_DestroyTexture (cr);
    // cairo_surface_write_to_png(surface, "burro_main_screen_present.png");
}

DEFINE_BOOL_FUNC_VOID(EngIsBlank, eng_is_blank)
DEFINE_VOID_FUNC_VOID(EngBlank, eng_blank)
DEFINE_VOID_FUNC_VOID(EngUnblank, eng_unblank)
DEFINE_BOOL_FUNC_VOID(EngIsColorswap, eng_is_colorswap)
DEFINE_VOID_FUNC_VOID(EngColorswap, eng_colorswap)
DEFINE_VOID_FUNC_VOID(EngUncolorswap, eng_uncolorswap)
DEFINE_DOUBLE_FUNC_VOID(EngGetBrightness, eng_get_brightness)
DEFINE_VOID_FUNC_DOUBLE(EngSetBrightness, eng_set_brightness)

JSFunctionSpec eng_functions[9] = {
DECLARE_BOOL_FUNC_VOID(EngIsBlank, eng_is_blank)
DECLARE_VOID_FUNC_VOID(EngBlank, eng_blank)
DECLARE_VOID_FUNC_VOID(EngUnblank, eng_unblank)
DECLARE_BOOL_FUNC_VOID(EngIsColorswap, eng_is_colorswap)
DECLARE_VOID_FUNC_VOID(EngColorswap, eng_colorswap)
DECLARE_VOID_FUNC_VOID(EngUncolorswap, eng_uncolorswap)
DECLARE_DOUBLE_FUNC_VOID(EngGetBrightness, eng_get_brightness)
DECLARE_VOID_FUNC_DOUBLE(EngSetBrightness, eng_set_brightness)
JS_FS_END
};
