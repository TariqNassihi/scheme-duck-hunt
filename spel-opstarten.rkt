
(#%require (only racket random error))
(load "positie.rkt")
(load "constanten.rkt")
(load "teken.rkt")
(load "levens.rkt")
(load "vogel.rkt")
(load "level.rkt")
(load "spel.rkt")
(load "wapen.rkt")
(load "Power-Up.rkt")

(define spel (maak-spel))

((spel 'start!))


