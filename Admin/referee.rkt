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
(require Trains/Lib/xsend)

(require SwDev/Debugging/spy) 

(module+ test
  (require rackunit))

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
  (define connections  (game-map-all-connections the-game-map))
  
  (let* ([the-state (setup-all-players the-external-players the-game-map player# RAILS-PER)]
         [the-state (play-turns the-state connections)]
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
      ['() (rstate good Cs drop-outs)]
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

#; {[Setof Destination] [Setof Destination] [Listof Destination]
                        ->
                        (U False [List [List Destination Destination] [Listof Destination]])}
;; determine whether the rejection is legitimate, compute the chosen ones and the remaining ones 
(define (legal-picks choose-from rejected all)
  (cond
    [(not (subset? rejected choose-from)) #false]
    [(not (= (set-count rejected) (- PICKS-PER DESTS-PER))) #false]
    [else
     (define chosen-ones (set->list (set-subtract choose-from rejected)))
     (define remaining   (remove* chosen-ones all))
     (list chosen-ones remaining)]))

(module+ test
  
  (check-equal?
   (legal-picks
    (set '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC])
    (set '[Boston DC] '[Boston NYC] '[NYC DC])
    (list '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC] '[DC Orlando]))
   '[[[Boston Chicago] [Boston Orlando]] ([Boston DC] [Boston NYC] [NYC DC] [DC Orlando])])

  (check-false
   (legal-picks
    (set '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC])
    (set '[Boston DC] '[Boston NYC] '[NYC Boston])
    (list '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC] '[DC Orlando])))

  (check-false
   (legal-picks
    (set '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC])
    (set '[Boston DC] '[Boston NYC])
    (list '[Boston Chicago] '[Boston Orlando] '[Boston DC] '[Boston NYC] '[NYC DC] '[DC Orlando]))))

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

#; {RefereeState [Listof Connection] -> RefereeState}
;; play turns until the game is over
;; separate drop-outs from  active to passive state
;; ASSUME all players have >= RAILS-MIN rails 
(define (play-turns the-state0 connections)
  (let play ([the-state the-state0][more 0])
    (define players (rstate-players the-state)) 
    (match players 
      ['() the-state]
      [_
       ;; the game may not end if all player keeps asking for more cards
       ;; and there are no cards 

       (match (play-1-turn the-state connections)
         [#false (play (rstate-drop the-state) 0)]
         [(list nu-the-state game-over?)
          (define more2 (if (equal? the-state nu-the-state) (add1 more) 0))
          (define next-state (rstate-rotate nu-the-state))
          (if (or game-over? (= more2 (length players)))
              (play-last-round next-state connections)
              (play next-state more2))])])))

#; {RefereeState [Listof Connections] -> [List RefereeState Boolean]}
;; allow each player to play one more turn, except the first one
(define (play-last-round the-state0 connections)
  (for/fold ([the-state (rstate-rotate the-state0)]) ((_ (rest (rstate-players the-state0))))
    (match (play-1-turn the-state connections)
      [#false       (rstate-drop the-state)]
      [(list nup _) (rstate-rotate nup)])))



#; {RefereeState [Listof Connection] -> (U False [List RefereeState Boolean])}
(define (play-1-turn the-state connections)
  (define next (first (rstate-players the-state)))
  (define the-player-state (rstate->pstate the-state))
  (match (xsend (ii-payload next) play the-player-state)
    [(? failed?) #false]
    [(? (curry equal? MORE))
     (if-rstate-update the-state (play-1-more-cards next (rstate-cards the-state)))]
    [response
     (and
      (legal-action? the-player-state connections response)
      (if-rstate-update the-state (player-1-acquire next response)))]))

#; {MePlayer [Listof Card] -> (U False [List MePlayer {listof Card}])}
(define (play-1-more-cards next remaining)
  (cond
    [(< (length remaining) CARDS-PER) (list next '[])]
    [else (define new-cards (take remaining CARDS-PER))
          (and (not (failed? (xsend (ii-payload next) more new-cards)))
               (list (ii+cards next new-cards) new-cards))]))

#; {MePlayer Response -> (U False [Listf MePlayer Boolean])}
(define (player-1-acquire next response)
  (let* ([iplayer next]
         [iplayer (ii-acquire iplayer response)])
    (list iplayer (ii-final? iplayer))))

;                                          
;                                          
;                                          
;     ;                       ;            
;     ;                       ;            
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;  
;     ;      ;  ;;  ;    ;    ;     ;    ; 
;     ;     ;    ;  ;         ;     ;      
;     ;     ;;;;;;  ;;;       ;     ;;;    
;     ;     ;          ;;;    ;        ;;; 
;     ;     ;            ;    ;          ; 
;     ;      ;      ;    ;    ;     ;    ; 
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;  
;                                          
;                                          
;                                          
;                                          

(module+ test
  
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples))

  (define buy-bos-sea (list 'Boston 'Seattle 'red 3))

  ;; -------------------------------------------------------------------------------------------------
  ;; player-1-acquire 
  (define total (game-map-all-connections vtriangle))
  (check-equal? (player-1-acquire ii-play buy-bos-sea) (list ii-final #t))
  
  (define (mock-1% x (y values))
    (class object%
      (define/public (more cards) (x cards))
      (define/public (play . x) (y buy-bos-sea))
      (super-new)))

  ;; -------------------------------------------------------------------------------------------------
  ;; player-1-more-cards 
  (define ([next nup] #:rails (rails 3) #:con (con '()) . cards)
    (ii 'x 'y rails (apply hash cards) (apply set con) nup))
  (define nup1 (next (new (mock-1% void))))
  (check-equal? (play-1-more-cards (nup1) (list 'red 'red)) (list (nup1 'red 2) '[red red]))
  (check-equal? (play-1-more-cards (nup1) (list 'red)) (list (nup1) '[]))

  (define nup2 (next (new [mock-1% (λ _ (raise 'bad))])))
  (check-false (play-1-more-cards (nup2) (list 'red 'red)))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; player-1-turn
  (define active1 (nup1 'red 3))
  (check-equal? 
   (play-1-turn (rstate (list active1) '[] '[]) (set buy-bos-sea))
   (list (rstate (list (nup1 #:rails 0 #:con `[,buy-bos-sea])) '[] '[]) #true))
  (define active2 (nup1))
  (check-false (play-1-turn (rstate (list active2) '[] '[]) (set buy-bos-sea)))

  (define active3 [(next (new [mock-1% void (λ _ (raise 'bad))]))])
  (check-false (play-1-turn (rstate (list active3) '[] '[]) (set buy-bos-sea)))

  (define active4 (next (new [mock-1% void (λ _ MORE)])))
  (check-equal? 
   (play-1-turn (rstate (list [active4]) '[red red] '[]) (set buy-bos-sea))
   (list (rstate (list (active4 'red 2)) '[] '[]) #false))

  ;; -------------------------------------------------------------------------------------------------
  ;; play-turns 
  (define active5 (new [mock-1% void (λ _ MORE)]))
  (define active6 (new [mock-1% void (λ _ (raise 'bad-o))]))
  (define (lop c (xternal active5)) (list ii-final (ii+payload (ii+cards ii-play c) xternal)))
  (define cards '[red red])
  (check-equal? (play-last-round (rstate (lop '[]) cards '()) (set)) (rstate (lop cards) '[] '[]))
  (check-equal? (play-last-round (rstate (lop '[] active6) '[] '()) (set))
                (rstate (list ii-final) '[] `[,(ii+payload ii-play active6)]))

  (check-equal? (play-turns (rstate (list (ii+payload ii-play active5)) cards '()) (set))
                (rstate (list (ii+cards (ii+payload ii-play active5) cards)) '[] '()))

  (define lop2 (list (ii+payload ii-final active5) (ii+payload ii-play active5)))
  (define lop2-r (list (ii+payload ii-play active5) (ii+cards (ii+payload ii-final active5) cards)))
  (check-equal? (play-turns (rstate lop2 cards '()) (set)) (rstate lop2-r '[] '[]))
  (check-equal? (play-turns (rstate '[] cards '()) (set)) (rstate '[] cards '()))

  (define lop3 (list (ii+payload ii-final active6)))
  (check-equal? (play-turns (rstate lop3 cards '()) (set)) (rstate '[] cards lop3)))
                


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
  (define players  (rstate-players the-state))
  (define paths    (all-paths game-map))
  (define +conns   (score-connections players))
  (define +dests   (score-destinations +conns paths))
  (define +longest (score-longest-path +dests paths))
  (define ranking  (rank +longest))
  ;; --- rank and inform -- 
  (xinform ranking (rstate-drop-outs the-state)))

#; {type Scored = [Listof (Cons PlayerState N)]}
#; {Yype Rankings = [Listof [Listof XPlayer]]}

#; {[Listof PlayerSyaye] -> Scored}
(define (score-connections players)
  (for/list ([p players])
    (cons p (ii-conn-score p))))

#; {Scored [Listof Path] -> Scored}
(define (score-destinations +conns paths)
  (for/list ([p.s +conns])
    (match-define (cons p s) p.s)
    (cons p (+ s (ii-destinations-connected p paths)))))

#; {Scored [Listof Path] -> Scored}
(define (score-longest-path +dests paths)
  (define sorted-paths (sort paths < #:key length))
  #; {Path [Listof [Cons PlayerState N]] -> (U False Scored)}
  ;; do any of the `ii-players` cover the path `sp` with their connections:
  ;; -- grant all of them points
  ;; -- leave everyone else alone 
  (define (a-player-covers sp players)
    (let loop ([players players] [result '()] [found? #false])
      (match players
        ['() (and found? (reverse result))]
        [(cons (and p.s (cons p s)) others)
         (if (not (ii-path-covered? p))
             (loop others (cons p.s players) found?)
             (loop others (cons (cons p (+ s LONG-PATH)) players) #true))])))
  ;; --- now check whether it matters --- 
  (for*/first ([sp sorted-paths] [f (in-value (a-player-covers sp +dests))] #:when f)
    f))

#; {Scored -> Ranking}
(define (rank +longest)
  (define sorted (sort +longest < #:key cdr))
  (define grouped (group-by cdr sorted))
  (for/list ([group grouped])
    (for/list ([p.s group])
      (ii-payload (car p.s)))))

#; {Ranking [Listof XPlayer] -> [List Ranking [Listof XPlayer]]}
(define (xinform rankings drop-outs)
  (match rankings
    ['() (list rankings drop-outs)]
    [(cons prelim-winners losers)
     (define-values (goods bads) (xmap-send (λ (a) (xsend a win #true)) prelim-winners))
     (let loop ([losers losers] [rankings (list (map first goods))] [drops (append bads drop-outs)])
       (match losers
         ['() (list (reverse rankings) drops)]
         [(cons group others)
          (define-values (goods bads) (xmap-send (λ (a) (xsend a win #false)) group))
          (loop others (cons (map first goods) rankings) (append bads drops))]))]))
