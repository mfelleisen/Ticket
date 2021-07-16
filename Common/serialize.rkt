#lang racket

#; {type Graph = [Hashof String [Hashof String [Hashof String Natural]]]}
;; The hash table maps city names to city names, which are mapped to colors
;; and those map to the number of segments. All connections are bi-directional.
;; 
;; GURANATEE All connections between two cities A and B are specified in only
;; one hash entry (either from A to B or B to A). 

(require SwDev/Debugging/spy)

(define (guarantee h)
  (define all (hash->list h))
  (define dom (map car all))
  (define rng (map (λ (x) (map car (hash->list (hash-ref h x)))) dom))
  (for/and ([d dom] [r rng])
    (define ds (symbol->string d))
    (define rs (map symbol->string r))
    (andmap (λ (r) (string<? ds r)) rs)))

(define example
  [hash 'Seattle [hash 'Boston [hash 'red 3
                                     'green 4]
                       'Orlando [hash 'blue 5]]
        'Orlanod [hash 'Boston [hash 'white 3
                                     'green 5]]])

(define (graph? x) #t)

(require (only-in json jsexpr?))

(provide
 (contract-out 
  [graph->jsexpr (-> graph? (and/c jsexpr? guarantee))]))

;; ---------------------------------------------------------------------------------------------------
(require Trains/Common/board)

;; ---------------------------------------------------------------------------------------------------
(define (graph->jsexpr g)
  (define cities (graph-cities g))
  (for/hash ([c cities])
    (values c (to* g c))))

(define (to* graph city)
  (define steps (graph-steps graph city))
  (for/hash ([next (group-by to-city steps)])
    (define next-city (to-city (first next)))
    (define color+seg# (map (λ (x) (list (to-color x) (to-seg# x))) next))
    (values next-city (connected-via next))))

(define (connected-via next)
  (for/hash ([c (group-by to-color next)])
    (values (to-color (first c)) (to-seg# (first c)))))

(define (graph-steps graph city) (hash-ref graph city '[]))

(define (graph-cities g) (map car (hash->list g)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require (submod Trains/Common/board examples))
  
  (graph->jsexpr triangle))