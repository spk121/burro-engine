(define (burro audio)
  #:export ())

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
                                                         
(define (beep x y)
  (let ((square-wave-instrument (list->instrument (list 0.01 0.01 0.01
                                                        1.0 1.0 1.0 1.0
                                                        1.0 1.0
                                                        #f 0.5
                                                        1.0 0.0 0.33 0.0 0.2 0.0 0.14))))
    (let ((wav (instrument-generate-wave square-wave-instrument x 0.1 0.1)))
      (play-wave wav))
    (let ((wav (instrument-generate-wave square-wave-instrument y 0.1 0.1)))
      (play-wave wav))))

