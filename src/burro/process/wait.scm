(define-module (burro process wait)
  #:use-module (burro process base)
  #:export (wait-process))

;; A wait process.

;; It just waits.  It is useful only if you want to delay before you
;; start another process.

(define (on-update self delta-milliseconds)
  (base-process-on-update self delta-milliseconds)
  (when (get-active-flag self)
    (var-add! self 'start delta-milliseconds)
    (when (>= (var-ref self 'start) (var-ref self 'stop))
      (process-kill! self))))


(define (wait-process milliseconds)
  (let ((self (make-base-process)))
    (set-type! self PROC_WAIT)
    (var-set! self 'start 0)
    (var-set! self 'stop milliseconds)
    (set-on-update-func! self wait-process-on-update)
    self))
  
