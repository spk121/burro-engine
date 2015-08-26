;;; pram.scm - pre-allocated memory buffer assignment
;;;
;;; There is a constrained set of memory buffers.  Each memory buffer
;;; has a fixed size, and not all memory buffers have the same size.
;;; Some memory buffers can be used as in combination as a single
;;; larger memory buffer.

;;; This allocator finds the smallest available preallocated memory
;;; buffer (PRAM) to fit a given size.

(define-module (pram)
  #:use-module (srfi srfi-1)
  #:export (pram-alloc
            pram-free
            *pram-size*
            *pram-state*))

;; Keep this sorted by size
(define *pram-size*
  '((PRAM_F . 1024)
    (PRAM_G . 1024)
    (PRAM_H . 1024)
    (PRAM_I . 1024)
    (PRAM_J . 1024)
    (PRAM_E . 16384)
    (PRAM_A . 65536)
    (PRAM_B . 65536)
    (PRAM_C . 65536)
    (PRAM_D . 65536)
    (PRAM_0 . 262144)
    (PRAM_1 . 262144)))

(define *pram-multi-size*
  '((PRAM_FGHI 4096 (PRAM_F PRAM_G PRAM_H PRAM_I))
    (PRAM_AB 131072 (PRAM_A PRAM_B))
    (PRAM_CD 131072 (PRAM_C PRAM_D))
    (PRAM_ABCD 262144 (PRAM_A PRAM_B PRAM_C PRAM_D))
    ))

(define *pram-state*
  '((PRAM_0 . #f)
    (PRAM_1 . #f)
    (PRAM_A . #f)
    (PRAM_B . #f)
    (PRAM_C . #f)
    (PRAM_D . #f)
    (PRAM_E . #f)
    (PRAM_F . #f)
    (PRAM_G . #f)
    (PRAM_H . #f)
    (PRAM_I . #f)
    (PRAM_J . #f)))

(define (_pram_is_free sym)
  "Check to see if a PRAM buffer is free by checking to see if its
entry in the *pram-state* list is set to #f"
  (not (assv-ref *pram-state* sym)))

(define (_pram_reserve sym)
  "Label a PRAM buffer as reserved by setting its entry in the *pram-state*
list to the symbol used to reserve it."
  (set! *pram-state* (assv-set! *pram-state* sym sym)))

(define (_pram_multi_reserve children sym)
  (for-each (lambda (entry)
              (set! *pram-state* (assv-set! *pram-state* entry sym)))
            children))

(define (_pram_free sym)
  "Label a PRAM buffer as free by setting its entry in the *pram-state*
list to #f.  If this buffer was allocated as part of a multi-buffer
collection, the sibling buffers are also freed."
  (let ([state (assv-ref *pram-state* sym)])

    ;; If SYM is of a single buffer
    (if state
        (for-each (lambda (entry)
                    (if (eqv? state (assv-ref *pram-state* (first entry)))
                        (set! *pram-state* (assv-set! *pram-state* (first entry) #f))))
                  *pram-state*)

        ;; else if SYM is of a multi buffer, free all the children
        (let ([multi-state (assv-ref *pram-multi-size* sym)])
          (if multi-state
              (let ([children (second multi-state)])
                (for-each _pram_free children)))))))
  
(define (_sufficient sym desired-size)
  ;; First check the state alist to see if this sym is free
  (and (_pram_is_free sym)
       (<= desired-size (assv-ref *pram-size* sym))))

(define (_multi_sufficient sym desired-size)
  (let* ([info (assv-ref *pram-multi-size* sym)]
         [size (first info)]
         [children (second info)])
    (and (every _pram_is_free children)
         (<= desired-size size))))

(define (_pram_single_alloc size)
  "Returns the index of a single memory buffer that has at least SIZE
bytes, or #f if all the preallocated buffers are in use."

  ;; can through the single buffers for the first
  ;; free buffer of sufficient size
  (let ([entry (find (lambda (entry)
                       (_sufficient (car entry) size))
                     *pram-size*)])
    (if entry
        (let ([entry-sym (first entry)])
          (_pram_reserve entry-sym)
          entry-sym)
        ;; else
        #f)))

(define (_pram_multi_alloc size)
  "Returns the index of a compound memory buffer that has at least SIZE
bytes, or #f if all the preallocated buffers are in use."

  ;; Search through the *pram-multi-size* structure for
  ;; an entry that is free and is big enough
  (let ([entry (find (lambda (entry)
                       (_multi_sufficient (car entry) size))
                     *pram-multi-size*)])

    ;; If you find an entry that is free and big enough,
    ;; reserve all the sub-buffers and return its index
    (if entry
        (let ([entry-sym (first entry)]
              [entry-children-list (third entry)])
          (begin
            (_pram_multi_reserve entry-children-list entry-sym)
            entry-sym))
        ;; else
        #f)))


(define (pram-alloc size)
  "Returns the index of a single memory buffer or a group of memory
buffers that has at least SIZE bytes, or #f if all the preallocated
buffers are in use."

  ;; FIXME: handle multi-memory buffers
  (let ([single-pram (_pram_single_alloc size)])
    (if (not single-pram)
        (let ([multi-pram (_pram_multi_alloc size)])
          (if (not multi-pram)
              #f
              ;; else
              multi-pram))
        ;; else
        single-pram)))
  
(define (pram-free sym)
  "Given the INDEX of a memory buffer, this procedure marks the buffer
as free so that may be available in future calls of 'pram-alloc'"
  (_pram_free sym))
