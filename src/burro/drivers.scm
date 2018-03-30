(define-module (burro drivers)
  #:use-module (burro xml)
  #:use-module (burro engine)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-1)
  #:export (clickable-text))

(define (find-action actions index)
  "Searches in the action list for an entry that is active for index."
  (if (null? actions)
      #f
      ;; The actions list should have the form
      ;; ((start-index end-index thunk) ...)
      (let ((action-entry
	     (find (lambda (entry)
		     (and (<= (first entry) index)
			  (< index (second entry))))
		   actions)))
	(if action-entry
	    (third action-entry)
	    #f))))

(define (make-button-press-handler actions)
  "Creates a procedure that can be used as the button press callback
handler.  When called, it tries to convert x,y locations into
character locations.  If those character locations have associated
actions, they are activated."
  (lambda (time x y)
    ;; (format #t "BUTTON PRESS HANDLER ~s ~s ~s~%" time x y)
    (let ((index (mouse-position-to-string-index x y)))
      (when index
	(let ((action-thunk (find-action actions index)))
	  (when action-thunk
	    (action-thunk)))))))

(define (clickable-text burro-sxml-tree-inner)

  ;; We let the caller drop the uninteresting *TOP* node
  (let ((burro-sxml-tree
	 `(*TOP* ,burro-sxml-tree-inner)))
    ;; (format #t "BURRO SXML TREE ~s~%" burro-sxml-tree)
  (let ((actions (sxml-locate-actions burro-sxml-tree))
	(pango-sxml-tree (sxml-style-actions burro-sxml-tree)))
    ;; (format #t "ACTIONS ~S~%" actions)
    ;; (format #t "PANGO_SXML TREE ~S~%" pango-sxml-tree)
    (let ((pango-xml-string
	   (with-output-to-string
	     (lambda () (sxml->xml pango-sxml-tree)))))
      ;; (format #t "PANGO XML STRING ~S~%" pango-xml-string)
      ;; Write the string to the screen
      (set-markup pango-xml-string)
      
      ;; We'll need to hook a function up to receive button presses,
      ;; and then convert them into string indices.
      (receive-button-presses
       (make-button-press-handler actions))))))
