#lang racket

(provide
 connection/c
 connection
 connection?
 connection-from
 connection-to
 connection-ft
 connection-color
 connection-seg#
 connection-flip
 connection-ordered
 connection-good?

 connection-serialize)


(require (only-in Trains/Common/basic-constants color? seg#? list-cities))

(struct connection [from to color seg#] #:prefab)

(define (connection-flip x)
  (connection (connection-to x) (connection-from x) (connection-color x) (connection-seg# x)))

(define (connection-ordered x)
  (match x
    [(connection f t c s)
     (match-define [list x y] (list-cities f t))
     (connection x y c s)]
    [_ (error 'connection-ordered "connection expected, given: ~e" x)]))

(define (connection-ft c)
  (list (connection-from c) (connection-to c)))

(define (connection-good? x)
  (symbol<? (connection-from x) (connection-to x)))

(define connection/c (and/c connection? connection-good?))

;; ---------------------------------------------------------------------------------------------------

(define (connection-serialize c)
  (match-define [connection city1 city2 color seg#] c)
  (append (map ~a (list-cities city1 city2)) (list (~a color) seg#)))