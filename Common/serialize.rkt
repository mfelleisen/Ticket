#lang racket

#; {type JGraph = [Hashof String JSclie]}
#; {type JSlice = [Hashof String JColor]}
#; {type JColor = [Hashof String Natural]}
;; The hash table maps city names to city names, which are mapped to colors
;; and those map to the number of segments. All connections are bi-directional.
;; 
;; GURANATEE All connections between two cities A and B are specified in only
;; one hash entry (either from A to B or B to A). Use string<? to make this work.

(define (guarantee serialized-graph)
  (define dom (graph-cities serialized-graph))
  (define rng (map (λ (x) (map car (hash->list (hash-ref serialized-graph x)))) dom))
  (for/and ([d dom] [r rng])
    (andmap (λ (r) (symbol<? d r)) r)))

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
#; {Graph -> JGraph}
(define (graph->jsexpr graph)
  (define cities (graph-cities graph))
  (for/hash ([c cities])
    (values c (to* graph c))))

#; {Graph City -> JSlice}
(define (to* graph city)
  (for*/hash ([connection* (group-by to-city (graph-steps graph city))]
              [next-city   (in-value (to-city (first connection*)))]
              #:when (symbol<? city next-city))
    (define color+seg# (map (λ (x) (list (to-color x) (to-seg# x))) connection*))
    (values next-city (connected-via connection*))))

#; {[Listof Connection] -> JColors}
(define (connected-via connection*)
  (for/hash ([c (group-by to-color connection*)])
    (values (to-color (first c)) (to-seg# (first c)))))

#; {Graph City -> [Listof Connection]}
(define (graph-steps graph city) (hash-ref graph city '[]))

#; {Graph -> [Listof City]}
(define (graph-cities graph) (map car (hash->list graph)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require (submod Trains/Common/board examples))
  
  (graph->jsexpr triangle))