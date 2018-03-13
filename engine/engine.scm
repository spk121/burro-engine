;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN main.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruments
(define (square-wave-harmonic n)
  (if (even? n)
      0.0
      ;; else
      (/ 4.0 (* 3.1415 n))))

(define (triange-wave-harmonic n)
  (if (even? n)
      0
      ;; else
      (if (= 1 (modulo n 4))
          (/ 8.0 (* 3.14 3.14 n n))
          ;; else
          (- (/ 8.0 (* 3.14 3.14 n n))))))

(define square-wave-instrument (list->instrument '(0.01 0.01 0.01
                                                         1.0 1.0 1.0 1.0
                                                         1.0 1.0
                                                         #f 0.5
                                                         1.0 0.0 0.33 0.0 0.2 0.0 0.14)))
                                                         
(define %prev 0.0)

(define (rand lo hi)
  (+ lo (* (- hi lo)
           (/ (random 1000000) 1000000))))

(define (beep t)
  (let ((square-wave-instrument (list->instrument (list (rand 0.01 0.3) (rand 0.01 0.3) (rand 0.01 0.3)
                                                        (rand 0.8 1.2) (rand 0.8 1.2) 1.0 (rand 0.8 1.2)
                                                        (rand 0.5 1.0) (rand 0.5 1.0)
                                                        #f 0.5
                                                        1.0 0.0 0.33 0.0 0.2 0.0 0.14))))
    (let ((wav (instrument-generate-wave square-wave-instrument (rand 100.0 1000.0) 0.5 0.5)))
      (play-wave wav))))

(define (update x)
  (backdrop-set-color (logand #x7FFF (inexact->exact (floor (* x #b000010000000011)))))
  (console-move-to 0 0)
  (console-write-string (format #f "~10,4f ~10,4f ~10,4f     " (audio-time) (loop-time) (- (audio-time) (loop-time))))
  (when (< (+ 2 %prev) (audio-time))
        (beep (audio-time))
        (set! %prev (+ %prev 2)))
  #f)

(define BLAMMO
  (let ((bv (make-bytevector 44200)))
    (let loop ((i 0))
      (bytevector-s16-set! bv (* 2 i) (inexact->exact (round (* 10000 (+ (sin (* i 0.06)) (sin (* i 0.07)))))) (endianness little))
      (if (< i 22000)
          (loop (1+ i))
          bv))))
(play-wave BLAMMO)

(loop-set-idle-callback 'update)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END main.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

