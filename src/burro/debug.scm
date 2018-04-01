(define-module (burro debug)
  #:use-module (burro engine)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 pretty-print)
  #:export (console-info
	    console-error
	    watch))

(define-public *console-port*
  (make-custom-binary-output-port "console"
				  console-write-bytevector
				  #f #f #f))

(setvbuf *console-port* 'none)

(define (console-log-inner . x)
  (apply simple-format
	 (append (list *console-port*)
		 x)))

(define-public (Display x)
  "Writes a human-friendly representation of x to the developer
console"
  (display "\t" *console-port*)
  (display x *console-port*)
  (newline *console-port*))

(define-public (console-info message . args)
  "Writes an information icon and the MESSAGE to a new line on
developer console.  MESSAGE can contains '~A' and '~S' escapes.  When
printed, the escapes are replaced with the corresponding number of
ARGS.  '~A' is a human-friendly representation of the argument.  '~S'
is a more machine-friendly representation."
  (console-write-icon "dialog-information")
  (display "\t" *console-port*)
  (apply console-log-inner (append (list message) args))
  (newline *console-port*))

(define-public (warn . x)
  "Writes a warning icon and the MESSAGE to a new line on developer
console.  MESSAGE can contains '~A' and '~S' escapes.  When printed,
the escapes are replaced with the corresponding number of ARGS.  '~A'
is a human-friendly representation of the argument.  '~S' is a more
machine-friendly representation."
  (console-write-icon "dialog-warning")
  (display "\t" *console-port*)
  (apply console-log-inner x)
  (newline *console-port*))

(define-public (console-error . x)
  "Writes an error icon and the MESSAGE to a new line on developer
console.  MESSAGE can contains '~A' and '~S' escapes.  When printed,
the escapes are replaced with the corresponding number of ARGS.  '~A'
is a human-friendly representation of the argument.  '~S' is a more
machine-friendly representation."
  (console-write-icon "dialog-error")
  (display "\t" *console-port*)
  (apply console-log-inner x)
  (newline *console-port*))

(define-public (watch . stuff)
  "Writes the arguments passed to it to the developer console,
returning the last argument.  This is useful for tracing function
calls.  For example, you could replace a function call like

  (function arg)
  with
  (pk \"function returns\" (function arg))"
  (let ((stack (make-stack #t 2)))
    (debug-peek-append
     (object->string (car stuff))
     (with-output-to-string
       (lambda ()
	 (pretty-print
	  (car (last-pair stuff)))))
     (call-with-output-string
       (lambda (port)
	 (display-backtrace stack port 0 4))))))
