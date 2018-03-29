# Burro Engine Notes

## Using the REPL with Geiser

Things to remember:
- Use the `geiser-connect` command.
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
  the "sandbox".  This modules is anonymous, so you cannot switch to
  it using the `,m` command at the REPL.  You _can_ switch to them in
  the repl by typing
````
(set-current-module (get-sandbox))
````
- Every time you load a new game file, it is loaded into a newly
  created sandbox.

