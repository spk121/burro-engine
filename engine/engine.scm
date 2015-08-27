;; engine.scm -- main script for Burro engine scripting
;;
;; Copyright 2015  Michael L. Gran <spk121@yahoo.com>
;; GPL3+

;; This is the top level script for the game
;; engine.  Its primary job is to do initialization
;; and then set the on-idle and on-draw callback
;; functions.

(define ENGINE_SCRIPT_LOAD_TIME (strftime "%c" (localtime (current-time))))

(define (default-handler key . args)
  (write key) (newline)
  (write args) (newline)
  (backtrace))

;; INCLUDE MODULES
(use-modules (srfi srfi-1)
             (json))

(include-from-path "pram.scm")
(include-from-path "tmx.scm")

(define (engine-init)
  (backdrop-set-color 0 #xff334455)
  (backdrop-set-color 1 #xff554433)
  (bg-set-bmp-from-file 0 "brick_0.png")
  (bg-show 0))

(engine-init)


       
