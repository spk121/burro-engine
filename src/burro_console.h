#ifndef BURRO_CONSOLE_H
#define BURRO_CONSOLE_H

#include <gtk/gtk.h>

#define BURRO_TYPE_CONSOLE (burro_console_get_type())
G_DECLARE_FINAL_TYPE (BurroConsole, burro_console, BURRO, CONSOLE, G_OBJECT);

BurroConsole *burro_console_new();

// Console

enum burro_console_blink_index_tag
{
    CONSOLE_BLINK_NONE = 0,
    CONSOLE_BLINK_SLOW = 1,
    CONSOLE_BLINK_FAST = 2,
};

typedef enum burro_console_blink_index_tag burro_console_blink_index_t;

enum burro_console_color_index_tag
{
    CONSOLE_COLOR_DEFAULT = 0,
    CONSOLE_COLOR_BLACK = 1,
    CONSOLE_COLOR_RED = 2,
    CONSOLE_COLOR_GREEN = 3,
    CONSOLE_COLOR_YELLOW = 4,
    CONSOLE_COLOR_BLUE = 5,
    CONSOLE_COLOR_MAGENTA = 6,
    CONSOLE_COLOR_CYAN = 7,
    CONSOLE_COLOR_WHITE = 8,
    CONSOLE_COLOR_TRANSPARENT = 9
};

typedef enum burro_console_color_index_tag burro_console_color_index_t;

enum burro_console_intensity_index_tag
{
    CONSOLE_INTENSITY_NORMAL = 0,
    CONSOLE_INTENSITY_FAINT = 1,
    CONSOLE_INTENSITY_BOLD = 2
};

typedef enum burro_console_intensity_index_tag burro_console_intensity_index_t;

enum burro_console_polarity_index_tag
{
    CONSOLE_POLARITY_POSITIVE = 0,
    CONSOLE_POLARITY_NEGATIVE = 1,
};

typedef enum burro_console_polarity_index_tag burro_console_polarity_index_t;

enum burro_console_underline_index_tag
{
    CONSOLE_UNDERLINE_NONE = 0,
    CONSOLE_UNDERLINE_SINGLY = 1,
    CONSOLE_UNDERLINE_DOUBLY = 2
};

typedef enum burro_console_underline_index_tag burro_console_underline_index_t;

#endif
