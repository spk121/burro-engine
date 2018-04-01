(define-module (burro process call-procedure)
  #:use-module (burro engine)
  #:use-module (burro process base)
  #:export (call-procedure-process))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The call procedure process

;; It just runs a procedure and immediately quits

(define (on-update p delta-milliseconds)
  (base-process-on-update p delta-milliseconds)
  (process-kill! p)
  ((var-ref p 'procedure)))

(define (call-procedure-process proc)
  "Call a thunk and exit."
  (let ((self (make-base-process)))
    (set-type! self PROC_SCRIPTING)
    (var-set! self 'procedure proc)
    (set-on-update-func! self on-update)
    self))
