/* ecma48_test.c -- test console primitives */
#include "ecma48_test.hpp"
#include <string.h>
#include "jsapi.hpp"
#include "js_func.hpp"

int ecma48_execute(const char *data, int len);

static void ecma48_test(void)
{
    char do_bell[] = "\x07";
    ecma48_execute(do_bell, strlen(do_bell));
    
    char do_backspace[] = " \x08";
    ecma48_execute(do_backspace, strlen(do_backspace));

    char do_backtab[] = "\x1b" "[2Z";
    ecma48_execute(do_backtab, strlen(do_backtab));
}

DEFINE_VOID_FUNC_VOID(ecma48Test, ecma48_test);

JSFunctionSpec ecma48_test_functions[2] = {
    DECLARE_VOID_FUNC_VOID(ecma48Test, ecma48_test) 
    JS_FS_END
};
