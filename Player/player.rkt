#lang racket

(provide

 #; {type XPlayer}

 (contract-out
  [player% referee-player%/c]))

(require Trains/Common/player-interface)

(define player%
  (class object% [init-field strategy%]
    (field [strategy #false])

    [define/public (setup gm rails cards)
      (set! strategy (new strategy% [the-game-map gm] [rails# rails]))]

    [define/public (pick destinations)
      (send strategy pick-destinations destinations)]

    [define/public (play s)
      (send strategy choose-action s)]

    [define/public (more card-1 card-2)
      (void)]

    [define/public (win did-i-win?)
      (displayln `[me ,did-i-win?])]
    
    (super-new)))

(module+ test
  (require (submod ".."))
  (require Trains/Player/simple-strategy)
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples))

  (define p1 (new player% [strategy% simple-strategy%]))

  (send p1 setup vtriangle 45 '[red red blue blue])
  (send p1 pick '[(Boston Seattle)
                  (Boston Seattle)
                  (Boston Orlando)
                  (Boston Orlando)
                  (Orlando Seattle)])
  (send p1 play pstate1)
  (send p1 more 'green 'blue)
  (send p1 play pstate2)
  (send p1 win #false))