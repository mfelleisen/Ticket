#lang racket

;; an interface for strategies

;; -----------------------------------------------------------------------------
(provide strategy/c%)

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