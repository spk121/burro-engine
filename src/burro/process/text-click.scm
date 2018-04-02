(define-module (burro process text-click)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (burro process base)
  #:use-module (burro process call-procedure)
  #:use-module (burro engine)
  #:use-module (burro colors)
  #:export (text-click-process))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The text-click process

;; This waits for the user to click on certain character locations on
;; the screen.

;; It handles dynamic link mouse-over decoration.

(define-record-type hotspot
  (make-hotspot begin end action transition-time warming? ratio)
  hotspot?

  ;; Properties
  (begin          get-begin)
  (end            get-end)
  (action         get-action)
  (transition-time get-transition-time)
  (warming?  get-warming-state set-warming-state!)
  (ratio          get-ratio set-ratio!))

;; A hotspot is either warming (mouse is over the hotspot)
;; or it is cooling (mouse is not over the hotspot).

(define (update-hotspot hotspot delta-time)
  "Tweak the color ratios of the hotspot.  Return #t
if it changed or #f otherwise."
  (let ((delta (/ delta-time
		  (get-transition-time hotspot)))
	(old-ratio (get-ratio hotspot)))
    (if (get-warming-state hotspot)
	(set-ratio! hotspot
		    (min 1.0
			 (+ (get-ratio hotspot) delta)))
	;; else
	(set-ratio! hotspot
		    (max 0.0
			 (- (get-ratio hotspot) delta))))
    ;; (format #t "hotspot ratio ~a~%" (get-ratio hotspot))
    (not (= old-ratio (get-ratio hotspot)))))

(define (location-over-hotspot? hotspot location)
  (and (>= location (get-begin hotspot))
       (< location (get-end hotspot))))

(define (make-hotspot-list action-list transition-time)
  (map (lambda (action)
	 (make-hotspot (first action)	; codepoint index
		       (second action)	; codepoint end index
		       (third action)	; thunk
		       transition-time
		       #f
		       0.0))
       action-list))

(define (on-update self delta-milliseconds)
  (base-process-on-update self delta-milliseconds)
  (when (get-active-flag self)
    ;; Handle the movement of the mouse over text.
    (let ((location (var-ref self 'text-move)))
      (when location
	;; If there was a mouse-move event over text, check to see if
	;; the mouse is over a hotspot. When a mouse is over a
	;; hotspot, set it to warming; otherwise, set it to cooling.
	(for-each
	 (lambda (hotspot)
	   (set-warming-state! hotspot
			       (location-over-hotspot? hotspot location)))
	 (var-ref self 'hotspots))))
    ;; Adjust the brightness ratios of all the hotspots.  If they
    ;; change, change the color of the hotspot on the screen.
    (for-each
     (lambda (hotspot)
       (let ((changed? (update-hotspot hotspot delta-milliseconds)))
	 (when changed?
	   (let ((color (dissolve-color (var-ref self 'cold-color)
					(var-ref self 'hot-color)
					(get-ratio hotspot))))
	     ;; (format #t "disolve color ~x\n" color)
	     (update-text-fgcolor-on-region color
					    (get-begin hotspot)
					    (get-end hotspot))))))
     (var-ref self 'hotspots))
      ;; If the mouse has clicked on a hotspot, we're done: set the
      ;; output, and set that we're killed.
      (let ((location (var-ref self 'text-click)))
	(when location
	  (for-each
	   (lambda (hotspot)
	     (when (location-over-hotspot? hotspot location)
	       ;; A hotspot has been clicked.  We want to run the
	       ;; associated action, but, only after we've finished
	       ;; all the processes in this process chain.  So we add
	       ;; an action process to the end of this process chain.
	       (format #t "hotspot clicked\n")
	       ;; (pk "self" self)
	       ;; (pk "self->next" (process-get-next self))
	       (let loop ((p self))
		 (if (process-get-next p)
		     (loop (process-get-next p))
		     ;; else, we've reached the last process
		     (process-set-next! p
					(call-procedure-process (get-action hotspot)))))
	       (process-kill! self)))
	   (var-ref self 'hotspots))))))

(define (text-click-process location-list
			    cold-color-name
			    hot-color-name
			    transition-time)
  (let ((self (make-base-process)))
    (set-type! self PROC_CONTROL)
    (set-process-flags! self (logior PROCESS_FLAG_TEXT_MOVE
				     PROCESS_FLAG_TEXT_CLICK))
    (var-set! self 'hotspots (make-hotspot-list
			      location-list transition-time))
    (var-set! self 'cold-color (color cold-color-name))
    (var-set! self 'hot-color (color hot-color-name))
    (set-on-update-func! self on-update)
    self))
