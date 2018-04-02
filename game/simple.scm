(set-title "The Shortest Game")

(set-backdrop "dark slate grey")

(define (TheBeginning)
  (clickable-text
   `(markup
     "Every adventure starts at the beginning.\n"
     "Do you want to "
     (a (@ (action ,SetOut)) "set out")
     " or "
     (a (@ (action ,Quit)) "quit")
     ".")))

(define (SetOut)
  (clickable-text
   `(markup
     "You have left.\nYou are on an adventure.\n"
     (b "You are attacked!!\n")
     (a (@ (action ,Died)) "You have died."))))

(define (Quit)
  (clickable-text
   `(markup
     "You are a coward, but, you will live to see tomorrow.\n\n"
     (a (@ (action ,TheBeginning)) "Try again?"))))

(define (Died)
  (clickable-text
   '(markup
     (b "GAME OVER"))))

(TheBeginning)
