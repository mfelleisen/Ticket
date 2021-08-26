#lang racket

;; an interface for strategies

;; -----------------------------------------------------------------------------
(provide strategy/c% HOLD-10 BUY-NOW CHEAT)

;; -----------------------------------------------------------------------------
(require Trains/Common/basic-constants)
(require Trains/Common/map)
(require Trains/Common/state)
(require Trains/Common/player-interface)

;; -----------------------------------------------------------------------------
(define strategy/c%
  (class/c
   (init-field (the-game-map game-map?) (rails# natural?) (cards (listof color?)))
   (pick-destinations (->m (set/c any/c) (set/c any/c)))
   (choose-action     (->m pstate? (or/c string? action?)))))

(define HOLD-10 "Hold-10")
(define BUY-NOW "Buy-Now")
(define CHEAT   "Cheat")
