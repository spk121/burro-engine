;; This is the process manager.

;; It holds on to a list of process.  It delivers mouse events to the
;; processes, handles their lifetimes, and calls them on regular
;; intervals.

(define-module (burro pm)
  #:use-module (srfi srfi-1)
  #:use-module (burro process base)
  #:use-module (burro error)
  #:use-module (burro debug)
  #:export(pm-has-processes?
	   pm-update-or-error-string
	   pm-attach
	   pm-detach
	   pm-detach-all
	   pm-set-text-click
	   pm-set-mouse-click
	   pm-set-mouse-move))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *pm-text-click* #f)
(define *pm-mouse-click* #f)
(define *pm-mouse-move* #f)
(define *pm-text-move* #f)

(define (pm-set-text-click i)
  (set! *pm-text-click* i))
(define (pm-has-text-click?)
  (if *pm-text-click* #t #f))
(define (pm-get-text-click)
  *pm-text-click*)
(define (pm-clear-text-click)
  (set! *pm-text-click* #f))

(define (pm-set-text-move i)
  (set! *pm-text-move* i))
(define (pm-has-text-move?)
  (if *pm-text-move* #t #f))
(define (pm-get-text-move)
  *pm-text-move*)
(define (pm-clear-text-move)
  (set! *pm-text-move* #f))


(define (pm-set-mouse-click x y)
  (set! *pm-mouse-click* (list x y)))
(define (pm-has-mouse-click?)
  (if *pm-mouse-click* #t #f))
(define (pm-get-mouse-click)
  *pm-mouse-click*)
(define (pm-clear-mouse-click)
  (set! *pm-mouse-click* #f))

(define (pm-set-mouse-move x y)
  (set! *pm-mouse-move* (list x y)))
(define (pm-has-mouse-move?)
  (if *pm-mouse-move* #t #f))
(define (pm-get-mouse-move)
  *pm-mouse-move*)
(define (pm-clear-mouse-move)
  (set! *pm-mouse-move* #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The process manager procedures operate on the list of process
;; objects.

;; The process list is just a simple list of processes
(define *process-list* (list))

(define (pm-attach process)
  (console-info "Attaching new process")
  (set! *process-list*
    (append! *process-list*
	     (list process)))
  (process-set-attached! process #t))

(define (pm-detach process)
  "Detach a process from the process list, but, don't delete it."
  (set! *process-list*
    (remove! (lambda (x)
	       (eq? x process))
	     *process-list*))
  (process-set-attached! process #f))

(define (pm-is-process-type-active? type)
  "Are there any processes of this type?"
  (if (find (lambda (p)
	      (and (eqv? type (process-get-type p))
		   (or (not (process-is-dead? p))
		       ;; If a process is dead, but it has a child
		       ;; process, it still counts as active because
		       ;; the child will wake.
		       (process-get-next p))))
	    *process-list*)
      #t
      #f))

(define (pm-has-processes?)
  "true if there is anything going on."
  (not (null? *process-list*)))

(define (pm-delete-all)
  (for-each pm-detach *process-list*))

(define (pm-deliver-events-to-process process)
  (format #t "process flags ~x\n" (get-process-flags process))

  (when (logtest (get-process-flags process)
		 PROCESS_FLAG_MOUSE_MOVE)
    (if (pm-has-mouse-move?)
	(var-set! process 'mouse-move (pm-get-mouse-move))
	(var-set! process 'mouse-move #f)))
  (when (logtest (get-process-flags process)
		 PROCESS_FLAG_MOUSE_CLICK)
    (if (pm-has-mouse-click?)
	(begin
	  (format #t "pm delivers mouse clock ~a\n" (pm-get-mouse-click))
	  (var-set! process 'mouse-click (pm-get-mouse-click)))
	(var-set! process 'mouse-click #f)))
  (when (logtest (get-process-flags process)
		 PROCESS_FLAG_TEXT_CLICK)
    (if (pm-has-text-click?)
	(var-set! process 'text-click (pm-get-text-click))
	(var-set! process 'text-click #f)))
  (when (logtest (get-process-flags process)
		 PROCESS_FLAG_TEXT_MOVE)
    (if (pm-has-text-move?)
	(var-set! process 'text-move (pm-get-text-move))
	(var-set! process 'text-move #f))))


(define (pm-update delta-milliseconds)
  (when (pm-has-processes?)
    (for-each
     (lambda (p)
       (if (process-is-dead? p)
	   ;; Remove the process from the list.  Also, check for a
	   ;; child process and add it if it exists.
	   (begin
	     (when (process-get-next p)
	       (let ((child (process-get-next p)))
		 (process-set-next! p #f)
		 (pm-attach child)))
	     (pm-detach p))
	   ;; Otherwise, this is where the an active process gets
	   ;; called. This is where the magic happens.
	   (when (and (process-is-active? p)
		      (not (process-is-paused? p)))
	     (pm-deliver-events-to-process p)
	     (process-on-update p delta-milliseconds))))
     *process-list*))
  ;; Clear out any old mouse events
  (pm-clear-text-click)
  (pm-clear-mouse-click)
  (pm-clear-mouse-move)
  ;; Return whether there are any processes remaining.
  (pm-has-processes?))

(define (pm-update-or-error-string delta-milliseconds)
  (false-if-exception (pm-update delta-milliseconds)))
  ;;(error-string-if-exception (pm-update delta-milliseconds)))
