;; A cooperative multitasking process manager

(define-module (burro process)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-wait-process
	    pm-attach
	    pm-update
	    pm-update-or-error-string
	    pm-set-text-click
	    pm-set-mouse-click
	    pm-set-mouse-move))

(define PROC_BASE 0)
(define PROC_WAIT 1)
(define PROC_SPRITE 2)
(define PROC_CONTROL 3)
(define PROC_SCREEN 4)
(define PROC_MUSIC 5)
(define PROC_SOUNDFX 6)
(define PROC_SCRIPTING 7)

(define PROCESS_FLAG_ATTACHED #x0001)
(define PROCESS_RECEIVE_MOUSE_MOVE #x0002)
(define PROCESS_RECEIVE_MOUSE_CLICK #x0004)

(define-record-type process
  (make-process type)
  process?

  ;; Properties
  (type                get-type        set-type!)		; integer? symbol?
  (kill-flag           get-kill-flag   set-kill-flag!)				
  (active-flag         get-active-flag set-active-flag!)
  (paused-flag         get-paused-flag set-paused-flag!)
  (initial-update-flag get-initial-update-flag set-initial-update-flag!)
  (next                get-next set-next!)				; a reference to another process, or #f
  (process-flags       get-process-flags set-process-flags!)
  (variable-alist      get-variable-alist set-variable-alist!)

  ;; virtual methods
  ;; () → bool
  (is-dead             get-is-dead-func  set-is-dead-func!)
  ;; ()
  (kill                get-kill-func set-kill-func!)
  ;; () → integer?
  (type-getter         get-type-getter-func set-type-getter-func!)
  ;; (integer)
  (type-setter         get-type-setter-func set-type-setter-func!)
  ;; () → bool
  (is-active           get-is-active-func set-is-active-func!)
  ;; (bool)
  (active-setter       get-active-setter-func set-active-setter-func!)
  ;; () → bool
  (is-attached         get-is-attached-func set-is-attached-func!)
  ;; (bool)
  (attached-setter     get-attached-setter-func set-attached-setter-func!)
  ;; () → bool
  (is-paused           get-is-paused-func set-is-paused-func!)
  ;; ()
  (toggle-pause        get-toggle-pause-func set-toggle-pause-func!)
  ;; () → bool
  (is-initialized      get-is-initialized-func set-is-initialized-func!)
  ;; () → process
  (next-getter         get-next-getter-func set-next-getter-func!)
  ;; (process)
  (next-setter         get-next-setter-func set-next-setter-func!)
  ;; (delta-milliseconds)
  (on-update           get-on-update-func set-on-update-func!)
  ;; ()
  (on-initialize      get-on-initialize-func set-on-initialize-func!))

(define-syntax and=>
  (syntax-rules ()
    ((and=> proc val)
     (if proc
	 (proc val)
	 #f))
    ((and=> proc val1 val2)
     (if proc
	 (proc val1 val2)
	 #f))))


(define (process-is-dead? p)
  (and=> (get-is-dead-func p) p))
(define (process-kill! p)
  (and=> (get-kill-func p) p))
(define (process-get-type p)
  (and=> (get-type-getter-func p) p))
(define (process-set-type! p type)
  (and=> (get-type-setter-func p) p type))
(define (process-is-active? p)
  (and=> (get-is-active-func p) p))
(define (process-set-active! p flag)
  (and=> (get-active-setter-func p) p flag))
(define (process-is-attached? p)
  (and=> (get-is-attached-func p) p))
(define (process-set-attached! p want-attached)
  (and=> (get-attached-setter-func p) p want-attached))
(define (process-is-paused? p)
  (and=> (get-is-paused-func p) p))
(define (process-toggle-pause! p)
  (and=> (get-toggle-pause-func p) p))
(define (process-is-initialized? p)
  (and=> (get-is-initialized-func p) p))
(define (process-get-next p)
  (and=> (get-next-getter-func p) p))
(define (process-set-next! p next-process)
  (and=> (get-next-setter-func p) p next-process))
(define (process-on-update p delta-milliseconds)
  (and=> (get-on-update-func p) p delta-milliseconds))
(define (process-on-initialize p)
  (and=> (get-on-initialize-func p) p))

(define (var-ref p key)
  (assv-ref (get-variable-alist p) key))

(define (var-set! p key val)
  (let ((vars (get-variable-alist p)))
    (set-variable-alist! p
			 (assv-set! vars key val))))

(define (var-add! p key delta)
  (var-set! p key (+ delta (var-ref p key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A generic process

(define (base-process-is-dead? p)
  (get-kill-flag p))
(define (base-process-kill! p)
  (set-kill-flag! p #t))
(define (base-process-is-active? p)
  (get-active-flag p))
(define (base-process-get-type p)
  (get-type p))
(define (base-process-set-active! p flag)
  (set-active-flag! p flag))
(define (base-process-is-attached? p)
  (if (logand (get-process-flags p)
	      PROCESS_FLAG_ATTACHED)
      #t
      #f))
(define (base-process-set-attached! p want-attached)
  (if want-attached
      (set-process-flags! p (logior (get-process-flags p)
				    PROCESS_FLAG_ATTACHED))
      ;; else
      (set-process-flags! p (logand (get-process-flags p)
				    (lognot PROCESS_FLAG_ATTACHED)))))
(define (base-process-is-paused? p)
  (get-paused-flag p))
(define (base-process-toggle-pause! p)
  (set-paused-flag! p (not (get-paused-flag p))))
(define (base-process-is-initialized? p)
  (get-initial-update-flag p))
(define (base-process-get-next p)
  (get-next p))
(define (base-process-set-next! p next-process)
  (set-next! p next-process))
(define (base-process-on-update p delta-milliseconds)
  (when (get-initial-update-flag p)
    (let ((func (get-on-initialize-func p)))
      (when func
	(func p)))
    (set-initial-update-flag! p #f)))
  
(define (make-base-process)
  (let ((p (make-process PROC_BASE)))
    (set-kill-flag! p #f)
    (set-active-flag! p #t)
    (set-process-flags! p 0)
    (set-next! p #f)
    (set-paused-flag! p #f)
    (set-initial-update-flag! p #t)

    (set-is-dead-func! p base-process-is-dead?)
    (set-kill-func! p base-process-kill!)
    (set-type-getter-func! p base-process-get-type)
    (set-is-active-func! p base-process-is-active?)
    (set-active-setter-func! p base-process-set-active!)
    (set-is-attached-func! p base-process-is-attached?)
    (set-attached-setter-func! p base-process-set-attached!)
    (set-is-paused-func! p base-process-is-paused?)
    (set-toggle-pause-func! p base-process-toggle-pause!)
    (set-is-initialized-func! p base-process-is-initialized?)
    (set-next-getter-func! p base-process-get-next)
    (set-next-setter-func! p base-process-set-next!)
    (set-on-update-func! p base-process-on-update)
    (set-on-initialize-func! p #f)

    p))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A wait process.

;; It just waits.  It is useful only if you want to delay before you
;; start another process.

(define (wait-process-on-update p delta-milliseconds)
  (base-process-on-update p delta-milliseconds)
  (when (get-active-flag p)
    (var-add! p 'start delta-milliseconds)
    (when (>= (var-ref p 'start) (var-ref p 'stop))
      (process-kill! p))))


(define (make-wait-process milliseconds)
  (let ((self (make-base-process)))
    (set-type! self PROC_WAIT)
    (var-set! self 'start 0)
    (var-set! self 'stop milliseconds)
    (set-on-update-func! self wait-process-on-update)
    self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The fadeout process

;; This fades the screen to black, or the inverse.

(define (fade-process-on-update p delta-milliseconds)
  (base-process-on-update p delta-milliseconds)
  (when (get-active-flag p)
    (var-add! p 'start delta-milliseconds)
    (let ((start (var-ref p 'start))
	  (stop  (var-ref p 'stop)))
      (cond
       ((< start stop)
	(let ((intensity-ratio (/ (- stop start)
				  stop)))
	
	  (if (var-ref self 'fadeout)
	      ;; Fade out to black
	      (set-intensity intensity-ratio)
	      ;; else, fade from black into normal intensity.
	      (set-intensity (1- intensity-ratio)))))
       (else
	(process-kill! p))))))

(define (make-fade-process milliseconds fadeout?)
  "Fades the screen over MILLISCONDS of time.  If FADEOUT? is true, we
fade to black, if it is false, we do an inverse fade from black to
normal intensity."
  (let ((self (make-base-process)))
    (set-type! self PROC_SCREEN)
    (var-set! self 'start 0)
    (var-set! self 'stop milliseconds)
    (var-set! self 'fadeout fadeout?)
    (set-on-update-func! self fadeout-process-on-update)
    self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link highlighter processes

;; When the mouse is hovering on a clickable link, this gradually
;; brightens the clickable link from Color #1 to Color #2.  When the
;; mouse is not on a clickable link, it will gradually fade back to
;; Color #1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The process list is just a simple list of processes

(define *process-list* '())
(define *pm-text-click* #f)
(define *pm-mouse-click* #f)
(define *pm-mouse-move* #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The process manager procedures operat on the list of process
;; objects.

(define (pm-attach process)
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

(define (pm-delete-process-list)
  (for-each pm-detach *process-list*))

(define (pm-update delta-milliseconds)
  (when (pm-has-processes?)
    (for-each
     (lambda (p)
       (if (process-is-dead? p)
	   ;; Check for a child process and add it if it exists
	   (begin
	     (format #t "PROCESS IS DEAD\n")
	     (when (pk 'process-get-next (process-get-next p))
	       (let ((child (process-get-next p)))
		 (process-set-next! p #f)
		 (pm-attach child)))
	     (pm-detach p))
	   ;; Otherwise
	   (when (and (process-is-active? p)
		      (not (process-is-paused? p)))
	     (process-on-update p delta-milliseconds))))
     *process-list*))
  ;; Clear out any old mouse events
  (pm-clear-text-click)
  (pm-clear-mouse-click)
  (pm-clear-mouse-move)
  ;; Return if there are any processes remaining.
  (pm-has-processes?))

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

(define (pm-update-or-error-string delta-milliseconds)
  (error-string-if-exception (pm-update delta-milliseconds)))

(define (pm-set-text-click i)
  (set! *pm-text-click* i))
(define (pm-has-text-click?)
  (if *pm-text-click* #t #f))
(define (pm-get-text-click)
  *pm-text-click*)
(define (pm-clear-text-click)
  (set! *pm-text-click* #f))

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


  

