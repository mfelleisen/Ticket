#lang racket

;; a referee for supervising one game of Trains


;                                                          
;                                                          
;                                                          
;                                             ;            
;                                             ;            
;    ;;;;   ;   ;;  ; ;;;    ;;;;    ;;;;;  ;;;;;;   ;;;;  
;    ;  ;;   ;  ;   ;;  ;   ;;  ;;   ;;       ;     ;    ; 
;   ;    ;    ;;    ;    ;  ;    ;   ;        ;     ;      
;   ;;;;;;    ;;    ;    ;  ;    ;   ;        ;     ;;;    
;   ;         ;;    ;    ;  ;    ;   ;        ;        ;;; 
;   ;         ;;    ;    ;  ;    ;   ;        ;          ; 
;    ;       ;  ;   ;;  ;   ;;  ;;   ;        ;     ;    ; 
;    ;;;;;  ;    ;  ; ;;;    ;;;;    ;         ;;;   ;;;;  
;                   ;                                      
;                   ;                                      
;                   ;                                      
;                                                          

(provide
 referee)

;                                                                                                  
;                                                                                                  
;        ;                                       ;                             ;                   
;        ;                                       ;                             ;                   
;        ;                                       ;                                                 
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ;;;;;;   ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;  
;    ;  ;;   ;  ;;  ;;  ;    ;  ;;  ;  ;  ;  ;  ;;   ;  ;;  ;;   ;   ;   ;     ;     ;  ;;  ;    ; 
;   ;    ;  ;    ;  ;    ;  ;    ;  ;  ;  ; ;    ;  ;    ;  ;    ;  ;          ;    ;    ;  ;      
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;  ;  ; ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;;;    
;   ;    ;  ;       ;    ;  ;       ;  ;  ; ;    ;  ;       ;    ;  ;          ;    ;          ;;; 
;   ;    ;  ;       ;    ;  ;       ;  ;  ; ;    ;  ;       ;    ;  ;          ;    ;            ; 
;    ;  ;;   ;      ;;  ;    ;      ;  ;  ;  ;  ;;   ;      ;    ;   ;   ;     ;     ;      ;    ; 
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;  ;  ;  ;;; ;   ;;;;;  ;    ;    ;;;   ;;;;;;;  ;;;;;   ;;;;  
;                   ;                                                                              
;                   ;                                                                              
;                   ;                                                                              
;                                                                                                  

(require Trains/Common/basic-constants)
(require Trains/Common/map)
(require Trains/Common/state)
(require Trains/Common/player-interface)
(require Trains/Admin/state)
(require Fish/Lib/xsend)

;                                                          
;                                                          
;                      ;;;                                 
;                     ;                                    
;                     ;                                    
;    ;;;;;   ;;;;   ;;;;;;   ;;;;    ;;;;;   ;;;;    ;;;;  
;    ;;      ;  ;;    ;      ;  ;;   ;;      ;  ;;   ;  ;; 
;    ;      ;    ;    ;     ;    ;   ;      ;    ;  ;    ; 
;    ;      ;;;;;;    ;     ;;;;;;   ;      ;;;;;;  ;;;;;; 
;    ;      ;         ;     ;        ;      ;       ;      
;    ;      ;         ;     ;        ;      ;       ;      
;    ;       ;        ;      ;       ;       ;       ;     
;    ;       ;;;;;    ;      ;;;;;   ;       ;;;;;   ;;;;; 
;                                                          
;                                                          
;                                                          
;                                                          

(define colored-cards0 '[])

;; -- at most 8 players, at least 2 distinct players
;; -- how to select enough destinations from the map
;;    -- demans total tie breaking
;;    -- selecting a map could make sure this works
;;    -- assume the select map allows this
;; -- need a systematic way to hand

;; the RefereeState temporarily assigns #false to internal players's destination fields 

#; {[Map [Listof XPlayer] -> (List [Listof [Listof XPlayer]] [Listof XPlayer])]}
(define (referee the-game-map the-external-players
                 ;; the next two parameters are for deterministic testing 
                 #; [Listof Card]
                 #:cards   (cards #false)
                 #; [[Listof Destination] -> [Listof Destination]]
                 #:shuffle (shuffle values))
  
  (define player#      (length the-external-players))
  (define destination* (shuffle (all-destinations the-game-map)))

  (let* ([the-state (setup-all-players the-external-players the-game-map player# RAILS-PER)]
         [the-state (play-turns the-game-map the-state)]
         [results   (score-game the-state)])
    results))

;                                                  
;                                                  
;                                                  
;                     ;                            
;                     ;                            
;    ;;;;    ;;;;   ;;;;;;          ;    ;  ; ;;;  
;   ;    ;   ;  ;;    ;             ;    ;  ;;  ;  
;   ;       ;    ;    ;             ;    ;  ;    ; 
;   ;;;     ;;;;;;    ;             ;    ;  ;    ; 
;      ;;;  ;         ;             ;    ;  ;    ; 
;        ;  ;         ;             ;    ;  ;    ; 
;   ;    ;   ;        ;             ;   ;;  ;;  ;  
;    ;;;;    ;;;;;     ;;;           ;;; ;  ; ;;;  
;                                           ;      
;                                           ;      
;                                           ;      
;                                                  

#; {[Listof XPlayer] Map [Listof Card] [Listof Desinations] -> RefereeState}
;; inform players about the beginning of the game: opponents, rails, colored cards 
;; separate drop-outs from  active to passive state
(define (setup-all-players externals0 game-map cards0 destinations0)
  (let loop ([externals externals0] [Cs cards0] [Ds destinations0] [good '()] [drop-outs '()])
    (match externals
      ['() (rstate-drop good Cs drop-outs)]
      [(cons xplayer others)
       (define cards (take Cs CARD0-PER))
       (match (xsend xplayer setup game-map RAILS-PER cards)
         [(? failed?) (loop others Cs Ds good (cons xplayer drop-outs))]
         [_ (define pick-from (apply set (take Ds PICKS-PER)))
            (match (xsend xplayer pick pick-from)
              [(? failed?) (loop others Cs Ds good (cons xplayer drop-outs))]
              [rejects
               (match (legal-picks pick-from rejects Ds)
                 [#false (loop others Cs Ds good (cons xplayer drop-outs))]
                 [(list (list destination-1 destination-2) remaining)
                  (define iplayer (ii destination-1 destination-2 RAILS-PER cards (set) xplayer))
                  (loop others (drop Cs CARD0-PER) remaining (cons iplayer good) drop-outs)])])])])))

;; improve the above with control 
(require (prefix-in ctrl: racket/control))

(define (setup-all-players-prime externals0 game-map cards0 destinations0)
  (let loop ([externals externals0] [Cs cards0] [Ds destinations0] [good '()] [drop-outs '()])
    (match externals
      ['() (rstate-drop good Cs drop-outs)]
      [(cons xplayer others)
       (ctrl:prompt
        (define (failed) (ctrl:abort (loop others Cs Ds good (cons xplayer drop-outs))))
        (define cards (take Cs CARD0-PER))
        (xsend xplayer setup game-map RAILS-PER cards failed)
        (define pick-from (apply set (take Ds PICKS-PER)))
        (define rejects (xsend xplayer pick pick-from failed))
        (match-define (list (list d-1 d-2) remaining) (legal-picks pick-from rejects Ds failed))
        (define iplayer (ii d-1 d-2 RAILS-PER cards (set) xplayer))
        (loop others (drop Cs CARD0-PER) remaining (cons iplayer good) drop-outs))])))

#; {[Setof Destination] [Setof Destination] [Listof Destination]
                        ->
                        (U False [List [List Destination Destination] [Listof Destination]])}
;; determine whether the rejection is legitimate, compute the chosen ones and the remaining ones 
(define (legal-picks choose-from rejected all)
  (cond
    [(not (subset? rejected choose-from)) (values #false #false)]
    [(not (= (set-count rejected) (- PICKS-PER DESTS-PER))) (values #false #false)]
    [else
     (define chosen-ones (set->list (set-subtract choose-from rejected)))
     (define remaining   (remove* chosen-ones all))
     (list chosen-ones remaining)]))

;                                          
;                                          
;                                          
;     ;                                    
;     ;                                    
;   ;;;;;;  ;    ;   ;;;;;  ; ;;;    ;;;;  
;     ;     ;    ;   ;;     ;;   ;  ;    ; 
;     ;     ;    ;   ;      ;    ;  ;      
;     ;     ;    ;   ;      ;    ;  ;;;    
;     ;     ;    ;   ;      ;    ;     ;;; 
;     ;     ;    ;   ;      ;    ;       ; 
;     ;     ;   ;;   ;      ;    ;  ;    ; 
;      ;;;   ;;; ;   ;      ;    ;   ;;;;  
;                                          
;                                          
;                                          
;                                          

#; {Map RefereeState -> RefereeState}
;; play turns until the game is over
;; separate drop-outs from  active to passive state
(define (play-turns game-map the-state0)
  (let play ([the-state the-state0])
    (match (rstate-drop the-state)
      ['() the-state]
      [(cons next-player _)
       (match (play-1-turn next-player (rstate-drop the-state))
         [#false (play (rstate-drop the-state))]
         [(list player-state game-over?)
          (define next-state (rstate-drop the-state player-state))
          (if game-over? (play-last-round next-state) (play the-state))])])))

#; {Map RefereeState -> RefereeState}
;; allow each player to play one more turn, except the first one
(define (play-last-round game-map the-state0)
  (for/fold ([the-state (rstate-drop the-state0)]) ((p (rest (rstate-drop))))
    (match (play-1-turn game-map p the-state)
      [#false (rstate-drop the-state)]
      [result (rstate-rotate the-state (first result))])))

#; {Map Player PlayerState -> (U False [List PlayerState Boolean])}
(define (play-1-turn game-map next-player the-player-state)
  (match (xsend (ii-payload next-player) play the-player-state)
    [(? failed?)             #false]
    [(? (curry equal? MORE)) (play-1-more next-player)]
    [response                (player-1-acquire game-map next-player response)]))

#; {Map Player Response -> (U False [Listf PlayerState Boolean])}
(define (player-1-acquire game-map next-player response)
  (cond
    [(not (rstate-drop game-map next-player response)) #false]
    [else (define iplayer (ii-acquire next-player response))
          (list iplayer (ii-final? iplayer))]))

#; {Player -> (U False [List PlayerState Boolean])}
(define (play-1-more next-player)
  (define remaining (rstate-drop next-player))
  (cond
    [(< (length remaining) CARDS-PER) #false]
    [else (define new-cards (take remaining CARDS-PER))
          ((if (failed? (xsend (ii-payload next-player) more new-cards))
               #false 
               (list (ii+cards next-player new-cards) #false)))]))

;                                                          
;                                                          
;                                      ;                   
;                                                          
;                                                          
;    ;;;;     ;;;    ;;;;    ;;;;    ;;;    ; ;;;    ;;; ; 
;   ;    ;   ;   ;  ;;  ;;   ;;  ;     ;    ;;   ;  ;;  ;; 
;   ;       ;       ;    ;   ;         ;    ;    ;  ;    ; 
;    ;;;;   ;       ;    ;   ;         ;    ;    ;  ;    ; 
;        ;  ;       ;    ;   ;         ;    ;    ;  ;    ; 
;   ;    ;   ;   ;  ;;  ;;   ;         ;    ;    ;  ;;  ;; 
;    ;;;;     ;;;    ;;;;    ;       ;;;;;  ;    ;   ;;; ; 
;                                                        ; 
;                                                    ;  ;; 
;                                                     ;;;  
;                                                          
  
;; compute ranking from state, inform players of winning/losing 
;; separate drop-outs from  active to passive state
(define (score-game game-map the-state)
  (define players (rstate-players the-state))
  (define score-connections (for/list ([p players]) (cons p (ii-conn-score p))))
  (define the-paths (all-paths game-map))
  (define score-destinations
    (for/list ([p.s score-connections])
      (match-define (cons p s) p.s)
      (cons p (+ s ( ii-destinations-connected p the-paths)))))
  (define sorted-paths (sort all-paths < #:key length))
  (define score-longest-path
    (for*/first ([sp sorted-paths] [f (in-value (a-player-covers sp score-destinations))] #:when f)
      f))
  ;; --- inform and revise ranking --- 
  (define rankings (tie-break (sort score-longest-path < #:key cdr)))
  (xinform rankings (rstate-drop-outs the-state)))

#; {Path [Listof [Cons PlayerState N]] -> (U False [Listof [Cons PlayerState N]])}
;; do any of the `ii-players` cover the path `sp` with their connections 
(define (a-player-covers sp players)
  (define found? #false)
  (let loop ([players players] [result '()])
    (cond
      [(empty? players) (and found? (reverse result))]
      [else
       (define p.s (first players))
       (cond
         [(not (ii-path-covered? (car p.s))) (loop (rest players) (cons p.s players))]
         [else
          (set! found? #true)
          (loop (rest players) (cons (cons (car p.s) (+ (cdr p.s) LONG-PATH)) players))])])))

#; {[Listof [Cons PlayerState N]] -> [Listof [Listof XPlayer]]}
;; group by score then tie break among players with equal score if possible 
(define (tie-break sorted-by-score) 0)

#; {[Listof [Listof XPlayer]] [Listof XPlayer] -> [List [Listof [Listof XPlayer]] [Listof XPlayer]]}
(define (xinform rankings drop-outs)
  1)