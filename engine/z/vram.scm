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

(define VRAM_INDEX_LIST (list VRAM_0
                              VRAM_1
                              VRAM_A
                              VRAM_B
                              VRAM_C
                              VRAM_D
                              VRAM_E
                              VRAM_F
                              VRAM_G
                              VRAM_H
                              VRAM_I
                              VRAM_AB
                              VRAM_CD
                              VRAM_ABCD))

;; Construct an alist of indices and their VRAM sizes.
;; Make sure they are sorted from smallest to largest
(define (sort-by-size L)
  (stable-sort L
               (lambda (a b) (< (cdr a) (cdr b)))))

(define (vram-make-size-alist L)
  (map (lambda (index) (cons index (vram-get-u32-size index))) L))

(define VRAM_SIZE_LIST (sort-by-size (vram-make-size-alist VRAM_INDEX_LIST)))

;; Create a store that will be track to whom a VRAM bank is associated
(define VRAM_OWNER_LIST (make-list (length VRAM_INDEX_LIST) 'free))

;; Given a sorted alist of (index . size), find the first free
;; block of sufficient size
(define (is-free-block-of-sufficient-size index size)
  (let ([is-free-test (eqv? (list-ref index VRAM_OWNER_LIST) 'free)]
        [is-big-test (<= size (assoc-ref index VRAM_SIZE_LIST))]
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
               
(define (smallest-free-block matrix-size)
  (find (lambda (entry)
          (is-free-block-of-sufficient-size entry matrix-size))
        VRAM_INDEX_LIST))


(define (vram-allocate owner list-of-matrix-sizes)
  "Give a list of matrix sizes and an owner, this finds
the most appropriate VRAM blocks available.  It reserves them
for the given owner, and a list of the block IDs is returned.

OWNER, in this case is MAIN_BG, MAIN_OBJ, SUB_BG, or SUB_OBJ.

The list of sizes are matrix size indices, like MATRIX_16x16.

The return value will be a list of VRAM indices, like
(VRAM_A, VRAM_B).

Note that MAIN_OBJ and SUB_OBJ can only have memory area,
while MAIN_BG and SUB_BG can have up to five.

If no appropriate blocks can be found for the given matrix
sizes, an error will be thrown."
  #t)

(define (vram-deallocate owner)
  "The unreserves VRAM blocks previously reserved for a
given OWNER.

OWNER, in this case is MAIN_BG, MAIN_OBJ, SUB_BG, or SUB_OBJ"
  #t)
