#include <string.h>
#include "extern.h"
#include "commands.h"

#define MAX_COMMANDS_PER_STRING (100)

void do_commands_from_string (gchar *str, gsize length)
{
    gchar **tokens;
    int i = 0;

    tokens = xg_strsplit_set (str, "\r\n", MAX_COMMANDS_PER_STRING);
    while (tokens[i] != NULL)
    {
        if (strlen (tokens[i]) > 0)
        {
            xdebug("Dispatch %s", tokens[i]);
            if (xg_str_has_prefix (&(str[i]), "NOP"))
                /* Do nothing */
                ;
            else
                xerror("Unknown command %s", tokens[i]);
        }
        i ++;
    }
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

