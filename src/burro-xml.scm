;; The pango markup is
;; <span font="12.5"></span>
;; <b>
;; <big>
;; <i>
;; <s>
;; <sub>
;; <sup>
;; <small>
;; <tt>
;; <u>

;; And I am adding
;; <a>

(define-module (burro-xml)
  #:use-module (sxml match)
  #:use-module (sxml transform)
  #:use-module (srfi srfi-1)
  #:export (sxml-location-actions
	    sxml-style-actions))

;; (sxml transform) and (sxml fold) disagree on what 'foldts' is
;; supposed to do, so I define the (sxml fold) version here as
;; sxml-tree-fold.

;; Sometimes I look at Guile and know that, in a few weeks, I won't
;; understand what this does.  Everything in this file will fall into
;; that category, I think.

(define (atom? x)
  "Return #t if X is not a pair."
  (not (pair? x)))

(define (sxml-fold proc seed list)
  (if (null? list)
      seed
      (sxml-fold proc (proc (car list) seed)
		 (cdr list))))

;; This is just the canonical version of foldts.

(define (sxml-tree-fold fdown fup fhere seed tree)
  "This runs three procedures whilst traversing through the nodes of
an SXML tree.  FDOWN is called when descending; FUP when
ascending. FHERE is called when evaluating the different elements of a
node.

SEED is passed along from call to call, so it is where you
store anything you need as state.

TREE is the SXML tree."
  (cond
   ((atom? tree)
    (fhere seed tree))

   (else
    (fup seed
	 (sxml-fold (lambda (child child-seed)
		      (sxml-tree-fold fdown fup fhere
				      child-seed child))
		    (fdown seed tree)
		    tree)
	 tree))))

(define (sxml-locate-actions tree)
  "Given an SXML tree of Pango-flavored markup, compute the number of
codepoints in all the text therein.  Look for action tags, which have
the form '(a (@ (action lambda) ...) text ...), and figure out their
location in that text.

Returns a table of actions with their respective locations."
  (let ((i 0)
	(action-positions '())
	(actions '()))

    (define (fhere state atom)
      ;; If state is a list, and the last element of that list is one
      ;; of our valid Pango Markup tags, then atom should be a visible
      ;; string.  We count the characters in the string.  If this is
      ;; an ACTION tag, we store those character positions for the
      ;; action table.
      (if (null? state)
	  (list atom)
	  ;; else
	  (let ((tag (last state)))
	    (case tag
	      
	      ;; Pango tags
	      ((markup span b big i s sub sup small tt u)
	       ;; Count the characters in this tag.
	       (set! i (+ i (string-length atom))))
	      
	      ;; Custom tags
	      ((a)
	       ;; Save the range of characters that encompasses this
	       ;; action tag.
	       (set! action-positions
		 (append action-positions
			 (list (list i (+ i (string-length atom))))))
	       (set! i ( + i (string-length atom)))))
	    (cons atom state))))

    (define (fup parent-state last-child-state node)
      ;; When looking at NODE as a whole, if it is A -- the action tag
      ;; -- we need to extract the action.
      (sxml-match node
		  [(a (@ (action ,action) . ,attribs) ,text ...)
		   (set! actions (append actions (list action)))
		   node]
		  [,otherwise
		   node])
      (if (null? parent-state)
	  (reverse last-child-state)
	  (cons (reverse last-child-state) parent-state)))

    (define (fdown state node)
      ;; When going down the tree, we do nothing interesting.
      '())

    ;; Finally we begin
    (sxml-tree-fold
     fdown
     fup
     fhere
     '()
     tree)

    ;; Zip together the action-positions and the actions
    ;; into one handy list.
    (map (lambda (pos act)
	   (append pos (list act)))
	 action-positions actions)))

(define (sxml-style-actions tree)
  "Given an SXML tree of Pango-flavored markup but also with the
Burro-specific A action tags, replace those action tags with span or
underline tags, which are valid Pango markup.

If the A action tag has no attributes other than the action attribute,
underline is used.  Otherwise span is used with the remaining
attributes."
  (pre-post-order
   tree
   `((a . ,(lambda node
	     (sxml-match node
			 [(a (@ (action ,action) . ,attribs) ,text ...)
			  (if (null? attribs)
			      `(u ,text ...)
			      `(span ,(append '(@) attribs) ,text ...))])))
     (*text* . ,(lambda (tag txt) txt))
     (*default* . ,(lambda x x)))))

#|
(define example
  `(*TOP* (markup
	   "There is a "
	   (b "button!")
	   "Do you "
	   (a (@ (action ,(lambda () "die"))) "press it")
	   " or do you "
	   (a (@ (action ,(lambda () "quit"))) "ignore it"))))

(pretty-print
 (sxml-locate-actions example))

(newline)

(pretty-print
 (sxml-style-actions example))

(newline)
|#
