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


