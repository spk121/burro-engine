(define-module (engine)
  #:use-module (burro)
  #:export (engine-init))

(define (engine-init)
  (backdrop-set-color 0 #xff334455)
  (backdrop-set-color 1 #xff554433)
  (bg-set-bmp-from-file 0 "brick_0.png")
  (bg-show 0))

(engine-init)
