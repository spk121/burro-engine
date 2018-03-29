# Burro Engine Notes

## Using the REPL with Geiser

Things to remember:
- Use the `geiser-connect` command.
- The scheme implementation is `guile`.
- The port is 37147.
- Load the Burro Engine primitives with

    (use-modules (burro)
	             (burro engine))
				 
- The game file is loaded into restricted, anonymous modules called
  "sandboxes".  These modules are anonymous, so you can't switch to
  them using the `,m` command at the REPL.  You can switch to them in
  the repl by typing

    (set-current-module (get-sandbox))

- Every time you load a new game file, it is loaded into a new
  sandbox.
  
