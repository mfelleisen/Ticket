#lang racket

(require Trains/Admin/referee)
(require Trains/Player/player)
(require Trains/Player/simple-strategy)
(require (submod Trains/Editor/map-editor homework))
(require Trains/Common/map)
(require (submod Trains/Common/map examples))
(require (prefix-in im: 2htdp/image))

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


(define (make-players i)
  (build-list i (λ (i) (new player% [strategy% simple-strategy%] [name (~a "player" i)]))))

(define the-map (construct-random-map (build-list 20 (compose string->symbol ~a)) 40 200 800))


#; {[Listof InternalConnection] [List String [List N N]] Image -> Image}
draw-connections
   
#; {[List String [List N N]] Image -> Image}
(define (external->internal-connections externals)
  (for/list ([c (in-set externals)])
    (list* (~a (connection-from c)) (~a (connection-to c)) (cddr c))))

(define cities (map (lambda (x) (cons (~a (first x)) (rest x))) (game-map-locations the-map)))
(define conns  (external->internal-connections (game-map-all-connections the-map))) 
(define +cities (draw-cities cities (im:empty-scene 200 800)))
(draw-connections conns cities +cities)


(define results (referee (make-players 8) the-map))

(display-results results)