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

  (let* ([the-state (setup-all-players the-external-players the-game-map player# RAILS-PER)]
         [the-state (play-turns the-state)])
    (end-game the-state)))

#; [N Map -> [Listof Destination]]
;; select "enough" destination cards so that every player can pick two 
(define (select-destination-sequence player# the-game-map)
  1)

; (struct ii [destination1 destination2 rails cards connections {payload #:mutable}] #:transparent)

#; {[Listof XPlayer] Map [Listof Card] [Listof Desinations] -> RefereeState}
;; inform players about the beginning of the game: opponents, rails, colored cards 
;; separate drop-outs from  active to passive state
(define (setup-all-players externals0 game-map cards0 destinations0)
  (let loop ([externals externals0] [Cs cards0] [Ds destinations0] [good '()] [drop-outs '()])
    (match externals
      ['() (rstate good Cs drop-outs)]
      [(cons xplayer others)
       (define cards (take Cs CARDS-PER))
       (match (xsend xplayer setup game-map RAILS-PER cards)
         [(? failed?) (loop others Cs Ds good (cons xplayer drop-outs))]
         [_ (define pick-from (take Ds PICKS-PER))
            (match (xsend xplayer pick pick-from)

              ;; I need a is-legal-destinations function:
              ;; the 3 destinations returned must be (a) distinct and (b) a subset of the given ones
              ;; the function should also factor out the 2 chosen ones
              ;; 
              ;; [Setof Destination] -> [Setof Destination]
              ;; 
              ;; the player's pick signature should be about sets not lists
              
              
              [(and !picked (list (? destination? -d1) (? destination? -d2) (? destination? -d3)))
               (match-define (list destination-1 destination-2) (remove !picked pick-from))
               (define iplayer (ii destination-1 destination-2 RAILS-PER cards (set) xplayer))
               (loop others (drop Cs CARDS-PER) (remove* !picked Ds) (cons iplayer good) drop-outs)]
              [(or (? failed?) _) (loop others Cs Ds good (cons xplayer drop-outs))])])])))

#; {RefereeState -> (va,ues RefereeState [Listof XPlayer])}
;; play turns until the game is over
;; separate drop-outs from  active to passive state
(define (play-turns the-state0)
  (let play ([the-state the-state0])
    (match the-state
      [(cons (and next-player) others)
       (play-1-turn next-player (rstate->pstate the-state))])))

#; {Player -> (values Player Boolean)}
(define (play-1-turn next-player the-player-state)
  (match next-player
    [(ii d-1 d-2 rails-left cards connections xplayer)
     #; (define response (xsend xplayer play the-state))
     #; (if (failed? response) ...)
     #; otherwise
     #; (if (equal? response MORE)
            -- check whether there are two cards
            -- if so, send them
            -- if not, skip)
     #; (action = [connection from to color seg#]  --> check legality)
     #; (if legal? execute cheat)
     4]))

#; {RefereeState -> (values [Listof [Listof Player]] [Listof XPlayer])}
;; compute ranking from state, inform players of winning/losing 
;; separate drop-outs from  active to passive state
(define (end-game the-state)
  5)