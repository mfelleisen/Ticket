#lang racket

;; an interface for strategies

;; -----------------------------------------------------------------------------
(provide strategy/c% HOLD-10 BUY-NOW CHEAT)

;; -----------------------------------------------------------------------------
(require Trains/Common/basic-constants)
(require Trains/Common/connection)
(require Trains/Common/map)
(require Trains/Common/state)
(require Trains/Common/player-interface)

;; -----------------------------------------------------------------------------
(define strategy/c%
  (class/c
   (init-field
    (the-game-map game-map?)
    (rails#       natural?)
    (cards        (listof color?)))

   ;; pick DESTS-PER cards from the given set for this game-map,
   ;; return the remaining cards 
   (pick-destinations (->m (set/c any/c) (set/c any/c)))

   ;; choose an action (acquire a connection, ask for cards) when granted a turn
   (choose-action     (->m pstate? (or/c (curry equal? MORE) connection/c)))))

(define HOLD-10 "Hold-10")
(define BUY-NOW "Buy-Now")
(define CHEAT   "Cheat")

;; -----------------------------------------------------------------------------
;; implemented strategies

#| Using the standard datatype pattern (see Fundamentals II, week 2): 


+-----------+                   +-----------+
| istrategy | <---------------- | astrategy |
+-----------+                   +-----------+
                                     ^
                                     |
                            +------------------+
                            |                  |
                +------------------+      +------------------+
                | hold-10-strategy |      | buy-now-strategy |
                +------------------+      +------------------+
                            ^
                            |
                +-------------------+
                | cheat-strategy    |
                +-------------------+

The filenames are the same as the class names. The `astrategy` class
uses Beta-style inheritance _and_ Java-style inheritance to set up
basic defaults 

|#

