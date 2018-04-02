;; The basic process structure in the cooperative multitasking process
;; manager

(define-module (burro process base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (PROC_BASE
	    PROC_WAIT
	    PROC_SPRITE
	    PROC_CONTROL
	    PROC_SCREEN
	    PROC_MUSIC
	    PROC_SOUNDFX
	    PROC_SCRIPTING
	    
	    PROCESS_FLAG_ATTACHED
	    PROCESS_FLAG_MOUSE_MOVE
	    PROCESS_FLAG_MOUSE_CLICK
	    PROCESS_FLAG_TEXT_CLICK
	    PROCESS_FLAG_TEXT_MOVE

	    make-process
	    process?
	    
	    get-type
	    set-type!
	    get-kill-flag
	    set-kill-flag!
	    get-active-flag
	    set-active-flag!
	    get-paused-flag
	    set-paused-flag!
	    get-initial-update-flag
	    set-initial-update-flag!
	    ;; get-next
	    ;; set-next!
	    get-process-flags
	    set-process-flags!
	    get-variable-alist
	    set-variable-alist!
	    get-is-dead-func
	    set-is-dead-func!
	    get-kill-func
	    set-kill-func!
	    get-type-getter-func
	    set-type-getter-func!
	    get-type-setter-func
	    set-type-setter-func!
	    get-is-active-func
	    set-is-active-func!
	    get-active-setter-func
	    set-active-setter-func!
	    get-is-attached-func
	    set-is-attached-func!
	    get-attached-setter-func
	    set-attached-setter-func!
	    get-is-paused-func
	    set-is-paused-func!
	    get-toggle-pause-func
	    set-toggle-pause-func!
	    get-is-initialized-func
	    set-is-initialized-func!
	    get-next-getter-func
	    set-next-getter-func!
	    get-next-setter-func
	    set-next-setter-func!
	    get-on-update-func
	    set-on-update-func!
	    get-on-initialize-func
	    set-on-initialize-func!

	    process-is-dead?
	    process-kill!
	    process-get-type
	    process-set-type!
	    process-is-active?
	    process-set-active!
	    process-is-attached?
	    process-set-attached!
	    process-is-paused?
	    process-toggle-pause!
	    process-is-initialized?
	    process-get-next
	    process-set-next!
	    process-on-update
	    process-on-initialize
	    
	    var-ref
	    var-set!
	    var-add!

	    base-process-is-dead?
	    base-process-kill!
	    base-process-is-active?
	    base-process-get-type
	    base-process-set-active!
	    base-process-is-attached?
	    base-process-set-attached!
	    base-process-is-paused?
	    base-process-toggle-pause!
	    base-process-is-initialized?
	    base-process-get-next
	    base-process-set-next!
	    base-process-on-update

	    make-base-process))

(define PROC_BASE 0)
(define PROC_WAIT 1)
(define PROC_SPRITE 2)
(define PROC_CONTROL 3)
(define PROC_SCREEN 4)
(define PROC_MUSIC 5)
(define PROC_SOUNDFX 6)
(define PROC_SCRIPTING 7)

(define PROCESS_FLAG_ATTACHED #x0001)
(define PROCESS_FLAG_MOUSE_MOVE #x0002)
(define PROCESS_FLAG_MOUSE_CLICK #x0004)
(define PROCESS_FLAG_TEXT_MOVE #x0008)
(define PROCESS_FLAG_TEXT_CLICK #x0010)

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
  


