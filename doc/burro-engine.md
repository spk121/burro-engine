# Burro Engine Notes

## The Game Script and the Sandbox

* The game script is loaded in one of two ways:
  * It can be loaded as a command line argument to `burro-engine`.
  * It can be loaded using the 'open' command from the GUI's gear menu.
* The game script is loaded into a restricted, anonymous module called
  the "sandbox". 
* The API in the sandbox has two sources:
  * Standard Scheme procedures come from the
    `all-pure-and-impure-bindings` list in `(ice-9 sandbox)`.
  * Burro Engine procedures come from the `sandbox-bindings` list in
    `(burro)`.

## Using the REPL with Geiser

When `burro-engine` is launched, the REPL TCP port is not enabled.  It
can be enabled under the "Developer Tools" widget's "REPL Server" tab.
Once enabled, it cannot be disabled without restarting the program.

Things to remember:

- From `emacs`, use the `geiser-connect` command.
- The scheme implementation is `guile`.
- The port is 37147.
- Load the Burro Engine primitives with

````
  (use-modules (burro)
               (burro engine))
````

- While the `(burro)` module is a standard module, the `(burro
  engine)` module is created in C by `burro-engine` and only exists
  when using it as an interpreter.
- The game script is loaded into a restricted, anonymous module called
  the "sandbox".  This module is anonymous, so you cannot switch to it
  using the `,m` command at the REPL.  Instead, use the following
  command at the REPL prompt

````
  (set-current-module (get-sandbox))
````

- Every time you load a new game file, it is loaded into a newly
  created sandbox.

## Notes on GTK

### Coding standard for GtkWidget

I just made this up, but, I needed something, because it is chaos.

- `#include` standard C headers
- `#include` local headers
- `#include` GTK, GLib, headers
- `#define` the constants
- `#define` preprocessor functions
- `enum`s
- `static` module variables
- The class structure
- `G_DEFINE_TYPE`
- `static` function declarations
- callback handlers and anything else
- the class overrides: `foo_init` `foo_dispose` `foo_delete`
- the event handlers: `foo_button_press_event`
- `foo_get_property` and `foo_set_property`
- class initialization
- any public C API, getters and setters
- Guile procedures
- Guile init

### How to stick the landing on a GtkWidget

In the `widget_class_init`, override the `GtkWidgetClass`
`delete_event` and the `GObjectClass` `dispose` overrideable method,
and maybe also the `GObjectClass` `finalize` method.

In the `GtkWidgetClass` `delete_event`, call `gtk_widget_destroy` on
self and return `TRUE`.

In the `GObjectClass` `dispose` overrideable method, call
`g_signal_handlers_disconnect_by_func()` to disconnect the signal
handlers, and `g_clear_object` to unref and free anything connected to
the widget.  After, chain up calling `dispose` on the parent class.

Note that methods may still be called after `dispose`, so every method
and Guile function needs to not segfault.

If necessary, in the `GObjectClass` `finalize` overrideable method,
free any remaining things in self that could otherwise cause a
segfault. After, chain up calling `finalize` on the parent class.
