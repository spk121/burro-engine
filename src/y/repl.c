//  This is the connection to the guile REPL
#include "../x.h"

SCM repl_server_socket = SCM_UNSPECIFIED;
SCM repl_server_object = SCM_UNSPECIFIED;

void
repl_init ()
{
    repl_server_socket = scm_c_eval_string("(make-tcp-server-socket #:port 37147)");
    repl_server_object = scm_call_1 (scm_c_eval_string ("spawn-coop-repl-server"),
                                         repl_server_socket);
}

void
repl_tick ()
{
    static int first = 1;
    static SCM func = SCM_BOOL_F;
    if (repl_server_object != SCM_UNSPECIFIED)
    {
        if (first == 1)
        {
            func = scm_c_eval_string ("poll-coop-repl-server");
            first = 0;
        }
        scm_call_1 (func, repl_server_object);
    }
}


