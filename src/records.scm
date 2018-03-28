;; Definition of DEFINE-RECORD-TYPE

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor . more) ...)
     (begin
       (define type
	 (make-record-type 'type '(field-tag ...)))
       (define constructor
	 (record-constructor type '(constructor-tag ...)))
       (define predicate
	 (record-predicate type))
       (define-record-field type field-tag accessor . more)
       ...))))

(define-syntax define-record-field
  (syntax-rules ()
    ((define-record-field type field-tag accessor)
     (define accessor (record-accessor type 'field-tag)))
    ((define-record-field type field-tag accessor modifier)
     (begin
       (define accessor (record-accessor type 'field-tag))
       (define modifier (record-modifier type 'field-tag))))))

;;(define record-marker (list 'record-marker))
(define record-marker 'record)

(define (make-record-type name field-tags)
  (let ((new (make-vector 2)))
    (vector-set! new 0 name)
    (vector-set! new 1 field-tags)
    new))

(define (record-type-name record-type)
  (vector-ref record-type 0))

(define (record-type-field-tags record-type)
  (vector-ref record-type 1))


(define (field-index type tag)
  (let loop ((i 1) (tags (record-type-field-tags type)))
    (cond ((null? tags)
	   (error "record type has no such field" type tag))
	  ((eq? tag (car tags))
	   i)
	  (else
	   (loop (1+ i) (cdr tags))))))

(define (record-constructor type tags)
  (let ((size (length (record-type-field-tags type)))
	(arg-count (length tags))
	(indexes (map (lambda (tag)
			(field-index type tag))
		      tags)))
    (lambda args
      (if (= (length args)
	     arg-count)
	  (let ((new (make-vector (+ size 2))))
	    (vector-set! new 0 record-marker)
	    (vector-set! new 1 type)
	    (for-each (lambda (arg i)
			(vector-set! new (1+ i) arg))
		      args
		      indexes)
	    new)
	  (error "wrong number of argumnents to constructor" type args)))))

(define (record? x)
  (and (vector? x)
       (< 0 (vector-length x))
       (eq? (vector-ref x 0) record-marker)))

(define (record-predicate type)
  (lambda (thing)
    (and (record? thing)
	 (eq? (vector-ref thing 1)
	      type))))

(define (record-accessor type tag)
  (let ((index (field-index type tag)))
    (lambda (thing)
      (if (and (record? thing)
	       (eq? (vector-ref thing 1)
		    type))
	  (vector-ref thing (1+ index))
	  (error "accessor applied to bad value" type tag thing)))))

(define (record-modifier type tag)
  (let ((index (field-index type tag)))
    (lambda (thing value)
      (if (and (record? thing)
	       (eq? (vector-ref thing 1)
		    type))
	  (vector-set! thing (1+ index) value)
	  (error "modifier applied to bad value" type tag thing)))))

(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(define-record-type <employee>
  (make-employee name age salary)
  employee?
  (name    employee-name)
  (age     employee-age    set-employee-age!)
  (salary  employee-salary set-employee-salary!))
