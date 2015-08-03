#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include "dl.h"

void *handle_gtk;

void (*_gtk_init)(int *argc, char **argv);

void init_dl ()
{
    char *error;

    handle_gtk = dlopen("libgtk-3.so", RTLD_LAZY);
    if (!handle_gtk) 
    {
        fprintf (stderr, "%s\n", dlerror ());
        exit (EXIT_FAILURE);
    }
    
    /* Clear existing errors */
    dlerror ();
    *(void **) (&_gtk_init) = dlsym (handle_gtk, "gtk_init");
    if ((error = dlerror()) != NULL) 
    {
        fprintf (stderr, "gtk_init(): %s\n", error);
        exit (EXIT_FAILURE);
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

