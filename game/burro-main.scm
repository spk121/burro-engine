(use-modules (burro passage))

(define (TheBeginning)
  (passage
   '("Every adventure starts at the beginning\n"
     "Do you want to "
     (action "set out" SetOut)
     " or "
     (action "quit" Quit)
     ".")))

(define (SetOut)
  (passage
   '("You have left. You are on an adventure. "
     (action "You have died." Died))))

(define (Quit)
  (passage
   '("You are a coward, but, you will live to see tomorrow")))

(define (Died)
  (passage
   '((b "GAME OVER"))))

(TheBeginning)
