#lang racket

(provide
 #; {[Listof X] -> Boolean}
 distinct?
 
 #; {[NEListof X] -> [NEListof X]}
 rotate)

;; -----------------------------------------------------------------------------
(require SwDev/Contracts/unique)
(module+ test (require rackunit))
;; -----------------------------------------------------------------------------

(define (rotate l)
  (match l
    [`(,fst ,snd ...) (append snd (list fst))]))
