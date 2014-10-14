#include <locale.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <SDL.h>
//#include <jsapi.h>
#include "Interpreter.hpp"
#include "ResourceFile.hpp"
#include "bg.hpp"
#include "eng.hpp"
#include "draw.hpp"
#include "lineedit.hpp"
#include "bg.hpp"
#include "loop.hpp"
#include "xiso9660.hpp"
#include "xsdl.hpp"
#include "tmx.hpp"
#include "Tmx.h"
#include "console.hpp"
int ecma48_init(void);


using namespace Tmx;

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
    SDL_LogSetAllPriority(SDL_LOG_PRIORITY_DEBUG);
    eng_initialize();

    // Temporary stuff
    // tmx_debug();

    string data_path{xsdl_get_data_path()};
    data_path.append("burro.iso");
    resource_file.Open(data_path);

    // Get a list of all the script files in the buffer
    auto script_filenames = resource_file.Get_resource_names_matching_suffix(".js");
    
    interpreter.Initialize();

    // Pass the list of script files names to the interpreter, for parsing
    // First load all the modules...
    for (auto script_filename : script_filenames)
        if (script_filename != "main.js")
            interpreter.Parse(script_filename);

    // Then the main script.
    interpreter.Parse("main.js");
    
    // In the main script, call function that reads 
    // vector<char> map_data = xiso9660_get_data(handle, "map1.tmx");
    //  Map map {};
    // map.ParseText(string(map_data.data(), map_data.size()));

    draw_initialize();
    lineedit_initialize();
    bg_set_brightness(1.0);
    
    console_reset();
    ecma48_init();
    loop.set_console_mode(true);
    loop.go();

    lineedit_finalize();
    draw_finalize();
    eng_finalize();
    interpreter.Finalize();

    atexit(SDL_Quit);
    
  return 0;
}
