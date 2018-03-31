;; burro.scm

;; Copyright (C) 2018   Michael L. Gran
;; This file is part of Burro Engine

;; Burro Engine is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Burro Engine is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Burro Engine.  If not, see <http://www.gnu.org/licenses/>.

(define-module (burro)
  #:use-module (burro drivers)
  #:use-module (burro engine)
  #:use-module (burro colors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 sandbox)
  #:re-export (clickable-text)
  #:export (make-sandbox
	    load-file-into-sandbox
	    eval-string-in-sandbox
	    call-with-limits))

(define-public *console-port*
  (make-custom-binary-output-port "console"
				  console-write-bytevector
				  #f #f #f))

(setvbuf *console-port* 'none)

#|
(define (console-log-inner . x)
  (display "\t" *console-port*)
  (when (> (length x) 0)
    (let loop ((cur (car x))
	       (rest (cdr x)))
      (display cur *console-port*)
      (if (not (null? rest))
	  (begin
	    (display " " *console-port*)
	    (loop (car rest)
		  (cdr rest)))))))
|#

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

(define-public (pk . stuff)
  "Writes the arguments passed to it to the developer console,
returning the last argument.  This is useful for tracing function
calls.  For example, you could replace a function call like

(function arg)
with
(pk \"function returns\" (function arg))" 
  (console-write-icon "dialog-information")
  (display "\tpk: " *console-port*)
  (write stuff *console-port*)
  (newline *console-port*)
  (car (last-pair stuff)))

(define sandbox-bindings
  '(((burro colors)
     color)

    ((burro drivers)
     clickable-text)
    
    ((burro engine)
     ;; From burro_app_win.c
     set-title
     ;; receive-clock-tick
     ;; register-game-loop-handler
     ;; From burro_canvas.c
     set-brightness
     get-brightness
     set-colorswap
     get-colorswap
     set-blank
     get-blank
     set-backdrop
     get-backdrop
     set-markup
     ;; from burro_canvas_vram.c
     VRAM_A
     VRAM_B
     VRAM_C
     VRAM_D
     VRAM_E
     VRAM_F
     VRAM_G
     VRAM_H
     VRAM_I
     VRAM_J
     get-vram-type
     get-vram-filename
     get-vram-image-size
     vram-get-u32-size
     load-image-file-into-vram)

    ((burro)
     ;; From burro.scm
     Display
     console-info
     warn
     console-error
     pk)))

;; On exception, returns a string with information about the error
(define-syntax error-string-if-exception
  (syntax-rules ()
    ((error-string-if-exception expr)
     (catch #t
       (lambda () expr)
       (lambda (key . args)
	 (call-with-output-string
	   (lambda (port)
	     (cond
	      ;; I guess I need to write special cases here for all
	      ;; the keys that aren't handled well by print-exception.
	      ((eqv? key 'limit-exceeded)
	       (let ((subr (car args))
		     (msg (cadr args))
		     (msgargs (caddr args)))
		 (if subr
		     (format port "In procedure ~a: "subr))
		 (apply format port msg (or args '()))))
	      (else
	       ;; print-exception is undocumented, but, useful.
	       (print-exception port #f key args))))))))))


(define (make-sandbox)
  (make-sandbox-module
   (append all-pure-and-impure-bindings
	   sandbox-bindings)))

(define *last-line-loaded* 0)
(define *last-expr-loaded* '())

(define (load-from-port-into-sandbox port sandbox)
  (set! *last-line-loaded* 0)
  (set! *last-expr-loaded* '())
  (let loop ((expr (read port)))
    (cond
     ((not (eof-object? expr))
      (when (integer? (port-line port))
	(set! *last-line-loaded* (port-line port)))
      (when expr
	(set! *last-expr-loaded* expr))
      (eval-in-sandbox expr #:module sandbox #:sever-module? #f)
      (loop (read port)))
     (else
      sandbox))))

(define (sandbox-load-error-handler key . args)
  "This handler tries to return a nicely formatted error string for
errors that happen when reading a file into the sandbox."
  (let ((exception-msg (call-with-output-string
			 (lambda (port)
			   (print-exception port #f key args)))))
    (string-append
     exception-msg
     "\nin expression\n"
     (object->string *last-expr-loaded*)
     "\nnear line "
     (number->string *last-line-loaded*))))

#|
(define (default-error-handler key . args)
  "This handler tries to return a nicely formatted error string for
general errors."
  (call-with-output-string
    (lambda (port)
      (cond
       ((eqv? key 'limit-exceeded)
	(let ((subr (car args))
	      (msg (cadr args))
	      (msgargs (caddr args)))
	  (if subr
	      (format port "In procedure ~a: "subr))
	  (apply format port msg (or args '()))))
       (else
	(print-exception port #f key args))))))
|#

(define (load-file-into-sandbox filename)
  "Read expressions from a file and evaluate them in SANDBOX.  If an
error occurs, a string is returned with a description of the problem."
  (let ((port (error-string-if-exception (open-input-file filename))))
    (if (string? port)
	port
	;; else
	(catch #t
	  (lambda ()
	    (let ((sandbox (make-sandbox)))
	      (load-from-port-into-sandbox port sandbox)
	      sandbox))
	  sandbox-load-error-handler))))

(define (eval-string-in-sandbox str sandbox)
  "Evaluates a string in a sandbox. If an error occurs, a string is
returned with a description of the problem."
  (error-string-if-exception
   (call-with-input-string str
     (lambda (port)
       (set-port-filename! port "console")
       (let loop ((expr (read port))
		  (result #f))
	 (cond
	  ((not (eof-object? expr))
	   (let ((val (eval-in-sandbox expr #:module sandbox #:sever-module? #f)))
	     (loop (read port)
		   val)))
	  (else
	   (object->string result))))))))

(define (call-with-limits proc . args)
  "Evaluates a procedure using the time and allocation limits
from the sandbox.  On exception, returns a string describing
the error."
  (error-string-if-exception
   (call-with-time-and-allocation-limits 0.1 #e10e6
					(lambda ()
					  (apply proc args)))))

