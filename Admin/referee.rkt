#lang racket

;; a referee for supervising one game of Trains

(require Trains/Common/basic-constants)
(require Trains/Common/state)
(require Trains/Admin/state)
(require Fish/Lib/xsend)

(define colored-cards0 '[])

;; -- at most 8 players, at least 2 distinct players
;; -- how to select enough destinations from the map
;;    -- demans total tie breaking
;;    -- selecting a map could make sure this works
;;    -- assume the select map allows this
;; -- need a systematic way to hand

;; the RefereeState temporarily assigns #false to internal players's destination fields 

#; {[Map [Listof XPlayer] -> (values [Listof [Listof XPlayer]] [Listof XPlayer])]}
(define (referee the-game-map the-external-players)
  
  (define player#      (length the-external-players))
  (define destination* (select-destination-sequence player# the-game-map))

  (let*-values ([(the-state) (setup-all-players the-external-players the-game-map player# RAILS-PER)]
                [(the-state) (pick-destination the-state destination*)]
                [(the-state) (play-turns the-state)])
    (end-game the-state)))

#; [N Map -> [Listof Destination]]
;; select "enough" destination cards so that every player can pick two 
(define (select-destination-sequence player# the-game-map)
  1)

; (struct ii [destination1 destination2 rails cards connections {payload #:mutable}] #:transparent)

#; {[Listof XPlayer] Map [Listof Card] [Listof Desinations] -> (values RefereeState [Listof XPlayer])}
;; inform players about the beginning of the game: opponents, rails, colored cards 
;; separate drop-outs from  active to passive state
(define (setup-all-players externals0 game-map cards0 destinations0)
  (let loop ([externals externals0] [Cs cards0] [Ds destinations0] [good '()] [drop-outs '()])
    (cond
      [(empty? externals) (rstate good Cs drop-outs)]
      [else
       (define xplayer  (first externals))
       (define response (xsend xplayer setup game-map RAILS-PER (take Cs CARDS-PER)))
       (cond
         [(failed? response)
          (loop (rest externals) Cs Ds good (cons xplayer drop-outs))]
         [else
          (define pick-from (take Ds DESTS-PER))
          (define response  (xsend xplayer pick pick-from))
          (cond
            [(failed? response)
             (loop (rest externals) Cs Ds good (cons xplayer drop-outs))]
            [else
             (define chosen (remove* response pick-from))
             (cond
               [(not (= (length chosen) 2))
                (loop (rest externals) Cs Ds good (cons xplayer drop-outs))]
               [else 
                (define remain (remove* response Ds))
                (define iplayer (ii (first chosen) (second chosen) RAILS-PER Cs (set) xplayer))
                (loop (rest externals) (drop Cs CARDS-PER) (cons iplayer good) drop-outs)])])])])))
    
#; {RefereeState [Listof Destination] -> (values RefereeState [Listof XPlayer])}
;; allow players to pick destination cards & record which one chose which 
;; separate drop-outs from  active to passive state
(define (pick-destination the-state destination-cards)
  3)

#; {RefereeState -> (va,ues RefereeState [Listof XPlayer])}
;; play turns until the game is over
;; separate drop-outs from  active to passive state
(define (play-turns the-state)
  4)

#; {RefereeState -> (values [Listof [Listof Player]] [Listof XPlayer])}
;; compute ranking from state, inform players of winning/losing 
;; separate drop-outs from  active to passive state
(define (end-game the-state)
  5)