#include <locale.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <SDL.h>
//#include <jsapi.h>
#include "js.hpp"
#include "bg.hpp"
#include "eng.hpp"
#include "draw.hpp"
#include "lineedit.hpp"
#include "bg.hpp"
#include "loop.hpp"

int unpacked_flag = 0;

int main(int argc, char** argv)
{
    int c;

    setlocale(LC_ALL, "");

    while ((c = getopt (argc, argv, "u")) != -1) {
        switch (c) {
        case 'u':
            unpacked_flag = 1;
            break;
        default:
            break;
        }
    }
    if (SDL_Init(SDL_INIT_VIDEO|SDL_INIT_TIMER) != 0) {
        fprintf(stderr, 
                "\nUnable to initialize SDL:  %s\n",
                SDL_GetError()
                );
        return 1;
    }
    
    eng_initialize();
    js_initialize(unpacked_flag);
    draw_initialize();
    lineedit_initialize();
    bg_set_brightness(1.0);
    
    loop_set_console_mode(1);
    // bg_set_bmp16_from_resource (0, "./r/spash_bmp16.tga");
    //bg_set (0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    //bg_set_priority (0, 0);
    //bg_show (0);
    
    // console_test_pattern ();
    loop();
    lineedit_finalize();
    draw_finalize();
    eng_finalize();
    js_finalize();

    atexit(SDL_Quit);
    
  return 0;
}
