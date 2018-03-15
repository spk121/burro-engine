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


(define tb (make-textbox "<b>Now</b> is the time for <i>all</i> good men to come to the aid of their country. 祇園精舎の鐘の声、諸行無常の響きあり。沙羅双樹の花の色、盛者必衰の理をあらはす。おごれる人も久しからず。ただ春の夜の夢のごとし。たけき者も遂にはほろびぬ、ひとへに風の前の塵に同じ ان صفحة التي مليارات عدد, بعض تم الأوروبية بالمطالبة."))


(define (update x)
  ;; (backdrop-set-color (logand #x7FFF (inexact->exact (floor (* 0.01 x #b000010000000011)))))
  ;; (console-move-to 0 0)
  ;;(console-write-string (format #f "~10,4f ~10,4f ~10,4f     " (audio-time) (loop-time) (- (audio-time) (loop-time))))
  
  (let ((mm (eng-get-button-press)))
    (when mm
      (console-move-to 0 0)
      (let* ((x (cadr mm))
	     (y (caddr mm))
	     (idx (textbox-xy-to-index tb x y)))
	(console-write-string (format #f "~10,4f ~10,4f ~a     " x y idx))
        (beep (cadr mm) (caddr mm)))))
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
(textbox-show tb)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END main.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

