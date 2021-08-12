#lang racket

;; a referee for supervising one game of Trains

;; TODO:
;; -- at most 8 players, at least 2 distinct players

;; The referee kicks out
;; -- cheating players
;; -- players that violate the (logical) contract in `Common/` (exn:fail:contract)
;; -- players that raise an exception (exn:fail)
;; -- players that take "too long" (accidental infinite loops)


;; The referee is abstract over two pieces to facilitate testing:
;; -- in what order to select (enough) destinations from the map for players to pick from;
;; -- in what order need a systematic way to hand cards to players.

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
 #; {[[Listof XPlayer] Map
                       ;; the next two optional parameters are for deterministic testing 
                       #:cards [Listof Card]
                       #:shuffle [[Listof Destination] -> [Listof Destination]]
                       ->
                       (List [Listof [Listof XPlayer]] [Listof XPlayer])]}
 referee)

(module+ examples ;; examples for setup, turns, and scoring
  (provide mock% mock-more-card ii-default make-an-ii cards lop))

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

(module+ examples
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples))
  (require rackunit))

;                                                                  
;                                                                  
;                                            ;;;                   
;                                              ;                   
;                                              ;                   
;    ;;;;   ;   ;;    ;;;   ;;;;;;  ; ;;;      ;     ;;;;    ;;;;  
;    ;  ;;   ;  ;    ;   ;  ;  ;  ; ;;  ;      ;     ;  ;;  ;    ; 
;   ;    ;    ;;         ;  ;  ;  ; ;    ;     ;    ;    ;  ;      
;   ;;;;;;    ;;     ;;;;;  ;  ;  ; ;    ;     ;    ;;;;;;  ;;;    
;   ;         ;;    ;    ;  ;  ;  ; ;    ;     ;    ;          ;;; 
;   ;         ;;    ;    ;  ;  ;  ; ;    ;     ;    ;            ; 
;    ;       ;  ;   ;   ;;  ;  ;  ; ;;  ;      ;     ;      ;    ; 
;    ;;;;;  ;    ;   ;;; ;  ;  ;  ; ; ;;;       ;;;  ;;;;;   ;;;;  
;                                   ;                              
;                                   ;                              
;                                   ;                              
;                                                                  

(module+ examples ;; examples for referee, setup, turns, and scoring
  (define (mock% #:setup (w void) #:pick (p values) #:play (y values) #:more (x void) #:win (z void))
    (class object% (init-field [strategy% 0] [name (gensym 'player)])
      (define/public (setup . x) (w x))
      (define/public (pick x) (p (apply set (take (set->list x) (- PICKS-PER DESTS-PER)))))
      (define/public (more cards) (x cards))
      (define/public (play . x) (y vtriangle-boston-seattle))
      (define/public (win . x) (z x))
      (super-new)))

  (define mock-more-card (new [mock% #:play (λ _ MORE)]))

  (define ([make-an-ii mock-inst] #:rails (rails 3) #:con (con '()) . cards)
    (ii '[x z] '[y z] rails (apply hash cards) (apply set con) mock-inst))
  (define ii-default  (make-an-ii (new (mock%))))
  
  (define (lop cards-p xternal (cards-f '[]))
    (list (ii+payload (ii+cards ii-final cards-f) mock-more-card)
          (ii+payload (ii+cards ii-play cards-p) xternal)))
  (define cards '[red red]))

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

(define (referee the-external-players the-game-map
                 #; [Listof Card]
                 #:cards   (cards (make-list 100 'red))
                 #; [[Listof Destination] -> [Listof Destination]]
                 #:shuffle (shuffle values))
  
  (define destination* (shuffle (all-destinations the-game-map)))
  (define connections  (game-map-all-connections the-game-map))
  
  ;; is this too expensive as a contract? 
  (unless (>= (length destination*) (* PICKS-PER (length the-external-players)))
    (error 'refree "not enough destinations"))

  (let* ([the-state (setup-all-players the-external-players the-game-map cards destination*)]
         [the-state (play-turns the-state connections)]
         [results   (score-game the-state the-game-map)])
    results))

(module+ test
  (define-values (m1 m2) (values (new (mock%)) (new (mock%))))
  (check-exn exn:fail? (λ () (referee [list m1 m2] vtriangle)))
  (check-equal? (referee [list m1 m2] vrectangle) `[ () [,m2 ,m1]]))

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
      ['() (rstate (reverse good) Cs drop-outs)] ;; age! 
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
                 [(list (list d-1 d-2) remaining)
                  (define iplayer (ii d-1 d-2 RAILS-PER (->hash cards) (set) xplayer))
                  (loop others (drop Cs CARD0-PER) remaining (cons iplayer good) drop-outs)])])])])))

#; {[Setof Destination] [Setof Destination] [Listof Destination]
                        ->
                        (U False [List [List Destination Destination] [Listof Destination]])}
;; determine whether the rejection is legitimate, compute the chosen ones and the remaining ones 
(define (legal-picks choose-from rejected all)
  (cond
    [(not (subset? rejected choose-from))                   #false]
    [(not (= (set-count rejected) (- PICKS-PER DESTS-PER))) #false]
    [else
     (define chosen-ones (set->list (set-subtract choose-from rejected)))
     (define remaining   (remove* chosen-ones all))
     (list chosen-ones remaining)]))

(define (->hash loc)
  (for/fold ([h (hash)]) ([c loc])
    (hash-update h c add1 0)))


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

  ;; -------------------------------------------------------------------------------------------------
  ;; set-up
  (define mock-bad-set   (new (mock% #:setup (λ _ (raise 'bad)))))
  (define vtriangle-apick (apply set (all-destinations vtriangle)))
  (define mock-good-pick (new (mock% #:pick (λ _ vtriangle-apick))))
  (define mock-bad-pick  (new (mock% #:pick (λ _ (raise 'bad)))))
  (define mock-ill-pick  (new (mock% #:pick (λ _ (set)))))
  
  (define setup-cards (make-list (* 2 #;players CARD0-PER) 'red))
  (define (make-ii-set ds)
    (ii (car ds) (cadr ds) RAILS-PER (->hash (make-list CARD0-PER 'red)) (set) mock-good-pick))
  (define-values (1dest 2dest)
    (let* ([dests (reverse vtriangle-dests)]
           [2dest (reverse (take dests DESTS-PER))]
           [dests (drop dests DESTS-PER)]
           [1dest (reverse (take dests DESTS-PER))])
      (values 1dest 2dest)))

  (check-equal? (setup-all-players (list mock-bad-set) vtriangle setup-cards vtriangle-dests)
                (rstate '[] setup-cards (list mock-bad-set)))
  (check-equal? (setup-all-players (list mock-ill-pick) vtriangle setup-cards vtriangle-dests)
                (rstate '[] setup-cards (list mock-ill-pick)))
  (check-equal? (setup-all-players (list mock-bad-pick) vtriangle setup-cards vtriangle-dests)
                (rstate '[] setup-cards (list mock-bad-pick)))

  (define 2goodies (list mock-good-pick mock-good-pick))
  (check-equal? (setup-all-players 2goodies vtriangle setup-cards vtriangle-dests)
                (rstate (list (make-ii-set 1dest) (make-ii-set 2dest)) '[] '[]))


  ;; -------------------------------------------------------------------------------------------------
  ;; legal-picks 
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

#; {RefereeState [Setof Connection] -> RefereeState}
;; play turns until the game is over
;; separate drop-outs from  active to passive state
;; ASSUME all players have >= RAILS-MIN rails 
(define (play-turns the-state0 connections)
  (let play ([the-state the-state0][turns-w/o-change 0])
    (define players (rstate-players the-state))
    (match players 
      ['() the-state]
      [_ (match (play-1-turn the-state connections)
           [#false (play (rstate-drop the-state) 0)]
           [(list nu-the-state game-over?)
            (define no-change (if (equal? the-state nu-the-state) (add1 turns-w/o-change) 0))
            (define next-state (rstate-rotate nu-the-state))
            (if (or game-over? (= no-change (length players)))
                (play-last-round next-state connections)
                (play next-state no-change))])])))

#; {RefereeState [Setof Connections] -> [List RefereeState Boolean]}
;; allow each player to play one more turn, except the first one
(define (play-last-round the-state0 connections)
  (for/fold ([the-state (rstate-rotate the-state0)]) ((_ (rest (rstate-players the-state0))))
    (match (play-1-turn the-state connections)
      [#false       (rstate-drop the-state)]
      [(list nup _) (rstate-rotate nup)])))

#; {RefereeState [Setof Connection] -> (U False [List RefereeState Boolean])}
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

(module+ test ;; tests for turns 
  
  ;; -------------------------------------------------------------------------------------------------
  ;; player-1-acquire 
  (check-equal? (player-1-acquire ii-play vtriangle-boston-seattle) (list ii-final #t))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; player-1-more-cards
  (define ii-bad-more (make-an-ii (new [mock% #:more (λ _ (raise 'bad))])))
  (check-equal? (play-1-more-cards (ii-default) cards) (list (ii-default 'red 2) cards))
  (check-equal? (play-1-more-cards (ii-default) (list 'red)) (list (ii-default) '[]))
  (check-false  (play-1-more-cards (ii-bad-more) cards))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; player-1-turn
  (define active1 (ii-default 'red 3))
  (check-equal? 
   (play-1-turn (rstate (list active1) '[] '[]) vtriangle-conns)
   (list (rstate (list (ii-default #:rails 0 #:con `[,vtriangle-boston-seattle])) '[] '[]) #true))
  (define active2 (ii-default))
  (check-false (play-1-turn (rstate (list active2) '[] '[]) vtriangle-conns))

  (define active3 [(make-an-ii (new [mock% #:play (λ _ (raise 'bad))]))])
  (check-false (play-1-turn (rstate (list active3) '[] '[]) vtriangle-conns))

  (define active4 (make-an-ii (new [mock% #:play (λ _ MORE)])))
  (check-equal? 
   (play-1-turn (rstate (list [active4]) cards '[]) vtriangle-conns)
   (list (rstate (list (active4 'red 2)) '[] '[]) #false))

  ;; -------------------------------------------------------------------------------------------------
  ;; play-last-round
  (define mock-bad-play  (new [mock% #:play (λ _ (raise 'bad-o))]))
  
  (define lop-ask-more (lop '[] mock-more-card))
  (define lop-got-more (lop cards mock-more-card))
  (define lop-bad-play (lop '[] mock-bad-play))
  
  (check-equal? (play-last-round (rstate lop-ask-more cards '()) (set)) (rstate lop-got-more '[] '[]))
  (check-equal? (play-last-round (rstate lop-bad-play '[] '()) (set))
                (rstate (list (first lop-bad-play)) '[] `[,(ii-payload (second lop-bad-play))]))

  ;; -------------------------------------------------------------------------------------------------
  ;; play-turns 
  (check-equal? (play-turns (rstate (list (ii+payload ii-play mock-more-card)) cards '()) (set))
                (rstate (list (ii+cards (ii+payload ii-play mock-more-card) cards)) '[] '()))
  
  (define lop-turn-got-more (reverse (lop '[] mock-more-card cards)))
  (define lop-turn-bad-play (list (ii+payload ii-final mock-bad-play)))
  
  (check-equal? (play-turns (rstate lop-ask-more cards '()) (set))
                (rstate lop-turn-got-more '[] '[]))
  (check-equal? (play-turns (rstate '[]  cards '()) (set))
                (rstate '[] cards '()))
  (check-equal? (play-turns (rstate lop-turn-bad-play cards '()) (set))
                (rstate '[] cards (map ii-payload lop-turn-bad-play))))

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


#; {Yype Rankings = [Listof [Listof XPlayer]]}

#; {RefereeState Map -> [List Ranking [Listof XPlayer]]}
;; compute ranking from state, inform players of winning/losing 
;; separate drop-outs from  active to passive state
(define (score-game the-state game-map)
  (match (rstate-players the-state)
    ['() (list '[] (rstate-drop-outs the-state))]
    [players
     (define +conns   (score-connections players))
     (define +dests   (score-destinations +conns game-map))
     (define +longest (score-longest-path +dests game-map))
     (define ranking  (rank +longest))
     ;; --- rank and inform -- 
     (xinform ranking (rstate-drop-outs the-state))]))

#; {type Scored = [Listof (Cons PlayerState N)]}

#; {[Listof PlayerState] -> Scored}
(define (score-connections players)
  (for/list ([p players])
    (cons p (ii-conn-score p))))

#; {Scored Map -> Scored}
(define (score-destinations +conns game-map)
  (for/list ([p.s +conns])
    (match-define (cons p s) p.s)
    (cons p (+ s (ii-destinations-connected p game-map)))))

#; {Scored Map -> Scored}
(define (score-longest-path +dests game-map)
  (define max-no-conns (apply max (map (compose set-count ii-connections car) +dests)))
  (define paths        (all-possible-paths game-map))
  (define filter-paths (filter (λ (p) (<= (length p) max-no-conns)) paths))
  (define sorted-paths (sort filter-paths > #:key length))
  
  #; {Path [Listof [Cons PlayerState N]] -> (U False Scored)}
  ;; do any of the `ii-players` cover the path `sp` with their connections:
  ;; -- grant all of them points
  ;; -- leave everyone else alone 
  (define (a-player-covers sp players)
    (let loop ([players players] [result '()] [found? #false])
      (match players
        ['() (and found? (reverse result))]
        [(cons (and p.s (cons p s)) others)
         (if (not (ii-path-covered? p sp))
             (loop others (cons p.s result) found?)
             (loop others (cons (cons p (+ s LONG-PATH))result) #true))])))
  ;; --- now check whether it matters --- 
  (for*/first ([sp sorted-paths] [f (in-value (a-player-covers sp +dests))] #:when f) f))

#; {Scored -> Ranking}
(define (rank +longest)
  (define sorted (sort +longest < #:key cdr))
  (define grouped (group-by cdr sorted))
  (for/list ([group grouped])
    (for/list ([p.s group])
      (ii-payload (car p.s)))))

#; {Ranking [Listof XPlayer] -> [List Ranking [Listof XPlayer]]}
(define (xinform rankings drops)
  (define (if-cons a d) (if (empty? a) d (cons a d)))
  (match rankings
    ['() (list rankings drops)]
    [(cons prelim-winners losers)
     (define-values (goods bads) (xmap-send (λ (a) (xsend a win #true)) prelim-winners))
     (let loop ([losers losers] [ranks (if-cons (map first goods) '[])] [drops (append bads drops)])
       (match losers
         ['() (list (reverse ranks) drops)]
         [(cons group others)
          (define-values (goods bads) (xmap-send (λ (a) (xsend a win #false)) group))
          (loop others (if-cons (map first goods) ranks) (append bads drops))]))]))

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
  
  (define lop2+score
    (list (cons (first lop-ask-more) 8)
          (cons (second lop-ask-more) 5)))
  (define lop3+score
    (list (cons (first lop-ask-more) (+ 8 (* 2 POINTS-PER)))
          (cons (second lop-ask-more) (- 5 (* 2 POINTS-PER)))))
  (define lop4+score
    (list (cons (first lop-ask-more) (+ 8 LONG-PATH (* 2 POINTS-PER)))
          (cons (second lop-ask-more) (- 5 (* 2 POINTS-PER)))))
  (define lop4-ranked
    [list (list (ii-payload (car (first lop4+score))))
          (list (ii-payload (car (second lop4+score))))])

  ;; -------------------------------------------------------------------------------------------------
  ;; score connections owned by players 
  (check-equal? (score-connections lop-ask-more) lop2+score)

  ;; -------------------------------------------------------------------------------------------------
  ;; score destinations of players 
  
  (check-equal? (score-destinations lop2+score vtriangle) lop3+score)

  ;; -------------------------------------------------------------------------------------------------
  ;; score longest path
  (check-equal? (score-longest-path lop3+score vtriangle) lop4+score)
  
  (define better (set '[Boston Seattle red 3] '[Orlando Seattle blue 5]))
  (define p1 (ii '[Boston Seattle] '[Boston Orlando] 32 (hash) better 'x))
  (define p2 (ii '[Boston Seattle] '[Boston Orlando] 32 (hash) (set '[Boston Seattle green 4]) 'y))
  (define p1-beats-p2 `[(,p2 . 0) (,p1 . ,LONG-PATH)])
  (check-equal? (score-longest-path `[(,p2 . 0) (,p1 . 0)] vtriangle) p1-beats-p2)

  ;; -------------------------------------------------------------------------------------------------
  ;; rank players 
  (define (ranked-ii i) (cons (ii '[x z] '[y z] 0 (hash) (set) (~a i)) (- 3 i)))
  (check-equal? (rank (cons (ranked-ii 2) (build-list 3 ranked-ii))) '[ ["2" "2"] ["1"] ["0"] ])
  (check-equal? (rank lop4+score) lop4-ranked)

  ;; -------------------------------------------------------------------------------------------------
  ;; inform winners, eliminate silly losers
  (check-equal? (xinform lop4-ranked '[]) [list lop4-ranked '()])
  (check-equal? (xinform '() '[]) [list '() '()])

  (define fail-on-win (new (mock% #:play values #:win (λ _ (raise 'bad)))))
  (check-equal? (xinform (cons (list fail-on-win) lop4-ranked) '[]) `[,lop4-ranked (,fail-on-win)])

  ;; -------------------------------------------------------------------------------------------------
  ;; `score-game`
  (check-equal? (score-game (rstate lop-ask-more '() '()) vtriangle) [list lop4-ranked '[]]))
