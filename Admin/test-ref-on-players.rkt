#lang racket

(require Trains/Admin/referee)
(require Trains/Player/player)
(require Trains/Player/simple-strategy)
(require (submod Trains/Common/map examples))

#; {Ranking -> Void}
(define (display-results results)
  (for ([rank (first results)] [r (in-naturals)])
    (for ([p rank])
      (displayln `[,(get-field name p) placed ,(show-rank (+ r 1))]))))

#;{N -> String}
(define (show-rank i)
  (case i
    [(1) "first"]
    [(2) "second"]
    [(3) "third"]
    [else (~a i "th")]))


(define p1 (new player% [strategy% simple-strategy%]))
(define p2 (new player% [strategy% simple-strategy%]))

(define results (referee (list p1 p2) vrectangle))

(display-results results)