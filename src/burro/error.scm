(define-module (burro error)
  ;; #:use-module (burro engine)
  #:export (string-if-exception))

(define (print-exception+ port frame key args)
  "Prints the exception described by key and args to the port.  Mostly
it just calls Guile's native print-exception, but it adds some keys
that print-exception doesn't handle."
  (cond
   ;; I guess I need to write special cases here for all
   ;; the keys that aren't handled well by print-exception.
   ((eqv? key 'limit-exceeded)
    (let ((subr (car args))
	  (msg (cadr args))
	  (msgargs (caddr args)))
      (if subr
	  (format port "In procedure ~a: " subr))
      (apply format port msg (or args '()))))
   (else
    ;; print-exception is undocumented, but, useful.
    (print-exception port frame key args))))

(define (find-a-location stack)
  (pk "stack" (list stack (stack-length stack)))
  (let loop ((index (1- (stack-length stack)))
	     (output ""))
    (pk "stack loop iter" index)
    (if (>= index 0)
	(let ((frame (stack-ref stack index)))
	  (pk "stack frame" frame)
	  ;; We want to find a frame with a name
	  (if (frame-procedure-name frame)
	      ;; Yeah! We found one
	      (loop (1- index)
		    (string-append output
				   " -> "
				   (object->string (frame-procedure-name frame))))
	      ;; Else, keep going up the stack
	      (loop (1- index)
		    output)))
	;; Else, we never found a frame with a nam
	output)))
   
;; On exception, returns a string with information about the error
(define-syntax string-if-exception
  (syntax-rules ()
    ((string-if-exception expr)
     (let ((stack #f))
       (catch #t
	 (lambda () expr)
	 
	 (lambda (key . args)
	   (let ((errstr (call-with-output-string
			   (lambda (port)
			     (print-exception+ port #f key args)))))
	     (string-append errstr
			    "\t"
			    (find-a-location stack))))
	 (lambda (key . args)
	   (set! stack (make-stack #t 1 1))))))))


#|
(define-syntax error-string-if-exception
  (syntax-rules ()
    ((error-string-if-exception expr)
     (catch #t
       (lambda () expr)
       (lambda (key . args)
	 (let ((errstr (call-with-output-string
			 (lambda (port)
			   (print-exception+ port #f key args))))
	       (stack (make-stack #t)))
	   
	   (debug-peek-append
	    (object->string expr)
	    errstr
	    (call-with-output-string
	      (lambda (port)
		(display-backtrace stack port))))
	   errstr))))))
|#
#|
(define (string-if-exception . expr)
  (let ((stack #f))
    (catch #t
      (lambda () expr)

      (lambda (key . args)
	(format #t "MLG BLAMMO\n")
	 (let ((errstr (call-with-output-string
			 (lambda (port)
			   (print-exception+ port #f key args)))))
	   (string-append "BLAMMO " errstr)))

       (lambda (key . args)
	 (set! stack (make-stack #t))))))
|#
