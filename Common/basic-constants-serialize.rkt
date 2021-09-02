#lang racket

;; de/serializing basic constants 

(provide
 parse-color*
 color*->jsexpr
 color*?)

;; ---------------------------------------------------------------------------------------------------

(define (color*->jsexpr loc) (map (λ (c) (~a c)) loc))

(define (color*? loc)
  (let/ec return
    (for/and ([c loc]) c)))

(define (parse-color* c)
  (map string->symbol c))
 