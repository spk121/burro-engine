;;; This is the matrix block allocator.

;;; Burro has some pre-allocated memory areas, which it calls VRAM,
;;; that are for the storage of 2D arrays of 32-bit unsigned integers.

;;; 2D uint32 arrays are used for three purposes: spritesheets,
;;; bitmaps, and map index arrays

;;; Each VRAM block can be reserved for either MAIN BG, MAIN OBJ,
;;; SUB BG, or SUB OBJ.

;;; BG consists of tilesheets, map index arrays, and bitmaps

;;; OBJ is just spritesheets

;;; The main allocator routine is (vram-allocate)


;;; SIMPLE BLOCKS
;;;  These blocks have a single owner
;;; VRAM_0, VRAM_1, VRAM_E, VRAM_F, VRAM_G, VRAM_H, VRAM_I

;;; COMPOUND BLOCKS
;;;  These blocks can be use individually or collectively
;;;  VRAM_A, VRAM_B, VRAM_C, VRAM_D, VRAM_AB, VRAM_CD, VRAM_ABCD

;; Construct an alist of indices and their VRAM sizes.
;; Make sure they are sorted from smallest to largest
(define (sort-by-size L)
  (stable-sort L
               (lambda (a b) (< (cdr a) (cdr b)))))

(define (vram-make-size-alist)
  (map (lambda (index) (cons index (vram-get-u32-size index))) VRAM_INDEX_LIST))

(define VRAM_SORTED_SIZE_ALIST (sort-by-size (vram-make-size-alist)))

;; Create a store that will be track to whom a VRAM bank is associated
(define VRAM_OWNER_LIST (make-list (length VRAM_INDEX_LIST) 'free))

;; Given a sorted alist of (index . size), find the first free
;; block of sufficient size
(define (is-free-block-of-sufficient-size vram-block-index vram-block-size required-size)
  (let ([is-free-test (eqv? (list-ref vram-block-index VRAM_OWNER_LIST) 'free)]
        [is-big-test (<= required-size vram-block-size)]
        [has-free-subblocks-test
         (or (and (eqv? index VRAM_AB)
                  (eqv? (list-ref VRAM_A VRAM_OWNER_LIST) 'free)
                  (eqv? (list-ref VRAM_B VRAM_OWNER_LIST) 'free))
             (and (eqv? index VRAM_CD)
                  (eqv? (list-ref VRAM_C VRAM_OWNER_LIST) 'free)
                  (eqv? (list-ref VRAM_D VRAM_OWNER_LIST) 'free))
             (and (eqv? index VRAM_ABCD)
                  (eqv? (list-ref VRAM_A VRAM_OWNER_LIST) 'free)
                  (eqv? (list-ref VRAM_B VRAM_OWNER_LIST) 'free)
                  (eqv? (list-ref VRAM_C VRAM_OWNER_LIST) 'free)
                  (eqv? (list-ref VRAM_D VRAM_OWNER_LIST) 'free))
             #t)])
    (and is-free-test
         is-big-test
         has-free-subblocks-test)))
               
(define (smallest-free-block-index required-size)
  (let ([available-entry (find (lambda (entry)
                             (is-free-block-of-sufficient-size (car entry) (cdr entry) required-size))
                           VRAM_SORTED_SIZE_ALIST)])
    (if (pair? available-entry)
        ;; if true, return just the index
        (car available-entry)
        ;; else
        #f)))

(define (vram-allocate owner list-of-matrix-sizes)
  "Given a list of matrix sizes and an owner, this finds
the most appropriate VRAM blocks available.  It reserves them
for the given owner, and a list of the block IDs is returned.

OWNER, in this case is MAIN_BG, MAIN_OBJ, SUB_BG, or SUB_OBJ.

The list of sizes are matrix size indices, like MATRIX_16x16.

The return value will be a list of VRAM indices, like
VRAM_A, VRAM_B.

Note that MAIN_OBJ and SUB_OBJ can only have memory area,
while MAIN_BG and SUB_BG can have up to five.

If no appropriate blocks can be found for the given matrix
sizes, an error will be thrown."

  (let ([modified-blocks '()])

    (define (allocate-free-block siz)
      (let ([block-index (smallest-free-block-index siz)])
        (when block-index
              (list-set! VRAM_OWNER_LIST block-index owner)
              (set! modified-blocks (append modified-blocks (list block-index))))))
  
    (for-each allocate-free-block list-of-matrix-sizes)
    
    (if (all modified-blocks)
        modified-blocks
        ;; else don't allocate any blocks and give up
        (begin
          (for-each (lambda (block-index)
                      (list-set! VRAM_OWNER_LIST block-index 'free))
                    modified-blocks)
          #f))))

(define (vram-deallocate owner)
  "The unreserves VRAM blocks previously reserved for a
given OWNER.

OWNER, in this case is MAIN_BG, MAIN_OBJ, SUB_BG, or SUB_OBJ"
  #t)
