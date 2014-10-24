#ifndef BURRO_COMMANDS_H
#define COMMANDS_H
#include <glib.h>

void do_commands_from_string (gchar *str, gsize length);
void initialize_command_parser (void);

#endif
