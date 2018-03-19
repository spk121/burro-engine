(define-module (burro markup)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (process-markup))

;; For textboxes, one can convert a list of SXML-like markup
;; into Pango-compliant Markup using the process-markup procedure

;; The input is a list of one or more of the following entry types
(define sample-markup
  '("text"
    ("text")
    ("text1" "text2" "etc")
    (b "bold text")
    (i "italic text")
    (action "text that you can click on" ACTION)))

;; If ACTION is a string, eval-string will be called on it
;; If ACTION is a thunk, it will be called

(define ACTION_TAG_START "<span fgcolor='#2020E0' underline='single'>")
(define ACTION_TAG_END "</span>")

(define (is-list-that-starts-with-string? x)
  (and (list? x)
       (not (null-list? x))
       (string? (first x))))

(define (merge-list-of-strings x)
  (apply string-append x))

(define (process-markup markup)
  "Given a markup list, this returns an Pango-flavor XML markup
string, and a list of actions."
  (let ((i 0)
        (output (string-copy ""))
        (actions '()))
    (map
     (lambda (entry)
       (match entry
         ;; Standalone strings
         ((? string? str)
          (set! output (string-append output str))
          (set! i (+ i (string-length str))))
         ;; List of strings
         ((? is-list-that-starts-with-string? strlist)
          (let ((str (merge-list-of-strings strlist)))
            (set! output (string-append output str))
            (set! i (+ i (string-length str)))))
         ;; b - Bold
         ((b str)
          (set! output (string-append output "<b>" str "</b>"))
          (set! i (+ i (string-length str))))
         ;; big - Makes font larger
         ((big str)
          (set! output (string-append output "<big>" str "</big>"))
          (set! i (+ i (string-length str))))
         ;; i - Italic
         ((i str)
          (set! output (string-append output "<i>" str "</i>"))
          (set! i (+ i (string-length str))))
         ;; s - Strikethrough
         ((s str)
          (set! output (string-append output "<s>" str "</s>"))
          (set! i (+ i (string-length str))))
         ;; sub - Subscript
         ((sub str)
          (set! output (string-append output "<sub>" str "</sub>"))
          (set! i (+ i (string-length str))))
         ;; sup - Superscript
         ((sup str)
          (set! output (string-append output "<sup>" str "</sup>"))
          (set! i (+ i (string-length str))))
         ;; small - Makes font smaller
         ((small str)
          (set! output (string-append output "<small>" str "</small>"))
          (set! i (+ i (string-length str))))
         ;; tt - Monospace font
         ((tt str)
          (set! output (string-append output "<tt>" str "</tt>"))
          (set! i (+ i (string-length str))))
         ;; u - Underline
         ((u str)
          (set! output (string-append output "<u>" str "</u>"))
          (set! i (+ i (string-length str))))
         ;; action 
         ((action str act)
          (set! output (string-append output
                                      ACTION_TAG_START
                                      str
                                      ACTION_TAG_END))
          (set! actions (append actions
                                `((,i ,(+ i (string-length str)) ,act))))
          (set! i (+ i (string-length str))))
         (_ #f)))
     markup)
    (list output actions)))
