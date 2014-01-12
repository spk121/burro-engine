;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN main.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize)
  ;;(bg-set-backdrop-color (BG-COLOR16-BLACK))
  (bg-set-backdrop-color #xEE)
  (bg-set-bmp16-from-resource (BG-MAIN-0) "/com/lonelycactus/burro/splash_bmp16.tga")
  (bg-set (BG-MAIN-0) 0.0 0.0 0.0 0.0 0.0 0.0)
  (bg-set-priority (BG-MAIN-0) 0)
  (bg-show (BG-MAIN-0))


  (display "Initialized...")
  (newline))

(define t 1.0)
(define (update x key)
  (bg-set-expansion (BG-MAIN-0) (+ 1.01 (sin x)))
  (display x) (display " ") (display key) (newline)
  (set! t x)
  #f)


(initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END main.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

