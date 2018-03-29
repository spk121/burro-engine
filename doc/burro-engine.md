# Burro Engine Notes

## Using the REPL with Geiser

Things to remember:
- Use the `geiser-connect` command.
- The scheme implementation is `guile`.
- The port is 37147.
- Load the Burro Engine primitives with
    (use-modules (burro)
	             (burro engine))
