/* xsdl2.cpp -- helper functions for the SDL2 library

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
#include <SDL.h>
#include "xsdl.hpp"

char *data_path = NULL;
char *pref_path = NULL;
bool paths_initialized = false;
bool write_enabled = false;

static void initialize_data_path()
{
    char *base_path = SDL_GetBasePath();
    if (base_path)
    {
        data_path = SDL_strdup(base_path);
        SDL_free(base_path);
    } 
    else 
        data_path = SDL_strdup("./");
}

static void initialize_pref_path(void)
{
    char *base_path = SDL_GetPrefPath("Lonely Cactus", "Project Burro");
    if (base_path)
    {
        pref_path = SDL_strdup(base_path);
        SDL_free(base_path);
        write_enabled =  true;
    }
    else
    {
        /* Do something to disable writing in-game */
    }
}

void xsdl_init_file_io (void)
{
    initialize_data_path();
    initialize_pref_path();
    paths_initialized = true;
}

const char *
xsdl_get_data_path(void)
{
    if (!paths_initialized)
        xsdl_init_file_io ();
    return data_path;
}

void
xSDL_Init_or_die (uint32_t flags)
{
    if (SDL_Init(flags) != 0) {
        fprintf(stderr,
                "\nUnable to initialize SDL:  %s\n",
                SDL_GetError()
            );
        exit (2);
    }
}

uint8_t
xsdl_read_uint8 (SDL_RWops* context)
{
    size_t sz;
    uint8_t buf;

    sz = SDL_RWread(context, &buf, sizeof(uint8_t), 1);

    SDL_assert(sz == 1);
    return buf;
}

size_t
xsdl_read_char_array (char *ptr, size_t len, SDL_RWops *istream)
{
    if (len == 0)
        return 0;

    return SDL_RWread(istream, ptr, sizeof(char), len);
}

size_t
xsdl_read_uint8_array (uint8_t *ptr, size_t len, SDL_RWops *istream)
{
    if (len == 0)
        return 0;

    return SDL_RWread(istream, ptr, sizeof(uint8_t), len);
}

size_t
xsdl_read_rle_array (void *data, size_t size, size_t count, SDL_RWops* istream)
{
    uint16_t x = 0;
    unsigned int i = 0;
    uint8_t packet_header;
    uint8_t packet_type;
    uint8_t packet_count;
    uint8_t packet_index;

    while (x < count * size)
    {
        /* This is a header byte */
        packet_header = xsdl_read_uint8 (istream);
        packet_type = packet_header & 0x80;
        packet_count = (packet_header & 0x7f) + 1;
        packet_index = 0;
        if (packet_type)
        {
            // This packet represents repeated values
            uint8_t repeated_value[4];
            for (i = 0; i < size; i ++)
                repeated_value[i] = xsdl_read_uint8 (istream);
            while (packet_index < packet_count)
            {
                for (i = 0; i < size; i ++)
                    ((uint8_t *)data)[x++] = repeated_value[i];
                packet_index ++;
            }
        }
        else
        {
            // This packet contains just raw values
            while (packet_index < packet_count)
            {
                for (i = 0; i < size; i ++)
                    ((uint8_t *)data)[x++] = xsdl_read_uint8 (istream);
                packet_index ++;
            }
        }
    }
    return x;
}

SDL_RWops *
xSDL_RWFromDataFile(const char *file, const char *mode)
{
    SDL_RWops *ret;
    char *fullpath;
    asprintf(&fullpath, "%s/%s", xsdl_get_data_path(), file);
    ret = SDL_RWFromFile(fullpath, mode);
    if (ret == NULL) {
        SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
                     "error in xSDL_RWFromFile: %s", 
                     SDL_GetError());
        exit(1);
    }
    return ret;
}
        
int64_t
xSDL_RWseek(SDL_RWops* context, int64_t offset, int whence)
{
    int64_t ret;
    ret = SDL_RWseek(context, offset, whence);
    SDL_assert(ret != -1);
    return ret;
}
