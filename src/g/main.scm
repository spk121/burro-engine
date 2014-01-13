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
  ;; void tone(int channel, guint64 start_time,
  ;;         double D_attack, double D_decay, double D_sustain, double D_release,
  ;;         double F_initial, double F_attack, double F_sustain, double F_release,
  ;;         double A_attack, double A_sustain,
  ;;         double duty, int noise, int waveform);
  (newline))

(define t #t)
(define t2 #t)
(define (update x key)
  (bg-set-expansion (BG-MAIN-0) (+ 1.01 (sin x)))
  (bg-set-backdrop-color (logand #x7FFF (inexact->exact (floor (* x #b000010000000011)))))
  ;; (display x) (display " ") (display key) (newline)
  ;; (set! t x)
  ;;(tone 0 0 0.1 0.1 0.3 0.1 1000.0 1000.0 1000.0 1000.0 1.0 0.7 0.5 0 0)
  (when t
          (tone 0 0 0.1 0.5 0.5 0.5 1000.0 440.0 440.0 800.0 1.0 0.7 0.5 1 1)
          (tone 1 (+ x 0.2) 0.1 0.5 0.5 0.5 523.0 523.0 523.0 800.0 1.0 0.7 0.5 1 1)
          (tone 4 (+ x 0.4) 0.1 0.5 0.5 0.5 250.0 659.0 659.0 800.0 1.0 0.7 0.5 1 1)
          (tone 2 (+ x 0.6) 0.1 0.5 0.5 0.1 250.0 880.0 880.0 800.0 1.0 0.7 0.5 1 1)
          (set! t #f))

  (when (and t2 (> x 3))
          (tone 0 0 0.1 0.5 0.5 0.5 1000.0 440.0 440.0 800.0 1.0 0.7 0.5 0 0)
          (tone 1 (+ x 0.2) 0.1 0.5 0.5 0.5 523.0 523.0 523.0 800.0 1.0 0.7 0.5 0 0)
          (tone 4 (+ x 0.4) 0.1 0.5 0.5 0.5 250.0 659.0 659.0 800.0 1.0 0.7 0.5 0 0)
          (tone 2 (+ x 0.6) 0.1 0.5 0.5 0.1 250.0 880.0 880.0 800.0 1.0 0.7 0.5 0 0)
          (set! t2 #f))

  #f)


(initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END main.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

