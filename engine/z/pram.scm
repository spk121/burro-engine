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
  (not (assv-ref *pram-state sym)))

(define (_pram_reserve sym)
  (set! *pram-state* (assv-set! *pram-state* sym #t)))

(define (_pram_free sym)
  (set! *pram-state* (assv-set! *pram-state* sym #t)))

(define (_sufficient sym desired-size)
  ;; First check the state alist to see if this sym is free
  (and (_pram_is_free sym)
       (<= desired-size (assv-ref *pram-size* sym))))

(define (_mutli_sufficient sym desired-size)
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
        (begin
          (_pram_reserve (car entry))
          (car entry))
        #f)))

(define (pram-alloc size)
  "Returns the index of a single memory buffer or a group of memory
buffers that has at least SIZE bytes, or #f if all the preallocated
buffers are in use."

  ;; FIXME: handle multi-memory buffers
  (_pram_single_alloc size))
  
(define (pram-free sym)
  "Given the INDEX of a memory buffer, this procedure marks the buffer
as free so that may be available in future calls of 'pram-alloc'"
  (_pram_free sym))
