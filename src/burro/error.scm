(define-module (burro error)
  #:use-module (burro engine)
  #:export (error-string-if-exception))

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
	  (format port "In procedure ~a: "subr))
      (apply format port msg (or args '()))))
   (else
    ;; print-exception is undocumented, but, useful.
    (print-exception port frame key args))))
  

;; On exception, returns a string with information about the error
(define-syntax error-string-if-exception
  (syntax-rules ()
    ((error-string-if-exception expr)
     (catch #t
       (lambda () expr)
       (lambda (key . args)
	 (let ((stack (make-stack #t))
	       (errstr (call-with-output-string
			 (lambda (port)
			   (print-exception+ port #f key args)))))
	   (string-append errstr "\n"
			  (call-with-output-string
			    (lambda (port)
			      (display-backtrace stack port))))))))))
;; (define-syntax error-string-if-exception
;;   (syntax-rules ()
;;     ((error-string-if-exception expr)
;;      (catch #t
;;        (lambda () expr)
;;        (lambda (key . args)
;; 	 (let ((errstr (call-with-output-string
;; 			 (lambda (port)
;; 			   (print-exception+ port #f key args))))
;; 	       (stack (make-stack #t)))
	   
;; 	 (debug-peek-append
;; 	  (object->string expr)
;; 	  errstr
;; 	  (call-with-output-string
;; 	    (lambda (port)
;; 	      (display-backtrace stack port))))
;; 	 errstr))))))
