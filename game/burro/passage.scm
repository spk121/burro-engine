(define-module (burro passage)
  #:use-module (srfi srfi-1)
  #:use-module (burro markup)
  #:use-module (burro)
  #:export (passage
            passage-default-idle-callback))

(define %passage-textbox-cur #f)
(define %passage-action-list-cur #f)

;; A PASSAGE is a term borrowed from Twine.  For our purposes, it is
;; the text draw in a Pango textbox on the main screen, plus a list of
;; actions that occur when a user clicks on certain words of that text.

(define (find-action idx)
  (format #t "in find action: ~s, list ~s~%" idx %passage-action-list-cur)
  (let ((action (find (lambda (entry)
                        (format #t "inner find action: ~s~%" entry)
                        (>= idx (first entry))
                        (< idx (second entry)))
                      %passage-action-list-cur)))
    (if action
        (third action)
        #f)))

(define (execute-action action)
  "Execution an ACTION, where ACTION is one of
- a procedure
- a symbol which is the name of a procedure in the current module
- a string which is the name of a procedure
- a string that starts and ends with parentheses that contains
  scheme code"
  (cond
   ((and (string? action)
         (not (string-null? action))
         (char=? (string-ref action 0) #\()
         (char=? (string-ref action (1- (string-length action))) #\)))
    ;; When action is a string that starts and ends with
    ;; parentheses, we assume the string is scheme code, and
    ;; we evaluate it.
    (eval-string action))

   ((and (string? action)
         (not (string-null? action))
         (not (string-index action #\space))
         (not (string-index action #\()))
    ;; When action is a string with no spaces or parentheses, we assume
    ;; the string is the name of a procedure, and we call it.
    (eval-string (string-append "(" action ")")))
   
   ((symbol? action)
    ;; When action is a symbol, we assume it is the name of
    ;; a procedure, and we call it
    ((module-ref (current-module) action)))

   ((procedure? action)
    ;; When acton is a procedure, we call it
    (action))

   (else
    ;; Do nothing
    #f)))
  

;; Most processing goes in a thunk set in loop-set-idle-callback
;; which runs ~ 60 time a second

(define (passage-default-idle-callback time-cur)
  "A callback that runs when GTK is idle.  Here is where we check if
the user has clicked on any hyperlinks."
  ;; We only respond to mouse clicks if a textbox is visible.
  (let ((textbox (get-current-textbox))
        (press (eng-get-button-press)))
    (when (and textbox press)
      (let* ((mouse-x (cadr press))
             (mouse-y (caddr press))
             (idx (textbox-xy-to-index textbox mouse-x mouse-y)))
        ;; If the user click on top of a glyph, IDX can be used to
        ;; discern which glyph was clicked.
        (when idx
          (let ((action (find-action (car idx))))
            (when action
              (execute-action action)))))))
  #f)

(define (passage markup)
  "This takes a Burro markup list MARKUP.  It sets the text on the
screen, and sets up mouse processing so that any actions described in
the markup are executed when clicked on."
  (let* ((xml&action (process-markup markup))
         (xml (car xml&action))
         (actions (cadr xml&action))
         (tb (make-textbox xml)))
    (textbox-show tb)
    (set! %passage-textbox-cur tb)
    (set! %passage-action-list-cur actions)
    (loop-set-idle-callback 'passage-default-idle-callback)
    #t))

