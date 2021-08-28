#lang racket/gui

;; a tournament manager that plays a complete tournament with players given ranked by "age"
;; and produces the list of first-placed players; it informs all non-cheaters whether they were
;; first-placed in the tournament

;; WARNING
;; the age-perservation is quite subtle, relying on `filter` over the original list of players
;; to re-org the players 

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
            
(require (only-in Trains/Common/player-interface manager-player/c))
(require Trains/Lib/list) 

(define player*/c [listof manager-player/c])
(define results/c (list/c player*/c player*/c))

(provide
 (contract-out
  (results/c contract?)
  [manager
   ;; produces a list consisting of the tournament winners and failures/cheaters 
   (->i ([lop (and/c player*/c cons? distinct?)])
        (#:shuffle [s (-> any/c any/c)]
         #:cards   [c (listof color?)])
        (r (or/c ERR results/c)))]))

(module+ examples
  (provide mock%))


;                                                                                      
;       ;                                  ;                                           
;       ;                                  ;                          ;                
;       ;                                  ;                                           
;    ;;;;   ;;;   ;;;;    ;;;   ; ;;    ;;;;   ;;;   ; ;;    ;;;    ;;;    ;;;    ;;;  
;   ;; ;;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;;  ;     ;   ;;  ;  ;   ; 
;   ;   ;  ;   ;; ;   ;  ;   ;; ;   ;  ;   ;  ;   ;; ;   ;  ;         ;   ;   ;; ;     
;   ;   ;  ;;;;;; ;   ;  ;;;;;; ;   ;  ;   ;  ;;;;;; ;   ;  ;         ;   ;;;;;;  ;;;  
;   ;   ;  ;      ;   ;  ;      ;   ;  ;   ;  ;      ;   ;  ;         ;   ;          ; 
;   ;; ;;  ;      ;; ;;  ;      ;   ;  ;; ;;  ;      ;   ;  ;;        ;   ;      ;   ; 
;    ;;;;   ;;;;  ;;;;    ;;;;  ;   ;   ;;;;   ;;;;  ;   ;   ;;;;   ;;;;;  ;;;;   ;;;  
;                 ;                                                                    
;                 ;                                                                    
;                 ;                                                                    

(require Trains/Common/basic-constants)
(require Trains/Common/map)
(require Trains/Admin/prepare-games)
(require Trains/Admin/referee)
(require Trains/Lib/xsend)
(require SwDev/Lib/should-be-racket)

(module+ examples
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples))
  (require (submod Trains/Admin/referee examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod ".."))
  (require (submod Trains/Common/map examples))
  (require rackunit))

;                                                   
;                                                   
;                                                   
;                                                   
;  ;;;;;;  ;;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;; 
;  ;  ;  ;     ;  ;;  ;      ;  ;;  ;  ;;  ;   ;;  ;
;  ;  ;  ;     ;  ;   ;      ;  ;   ;  ;   ;;  ;    
;  ;  ;  ;  ;;;;  ;   ;   ;;;;  ;   ;  ;;;;;;  ;    
;  ;  ;  ; ;   ;  ;   ;  ;   ;  ;   ;  ;       ;    
;  ;  ;  ; ;   ;  ;   ;  ;   ;  ;; ;;  ;       ;    
;  ;  ;  ;  ;;;;  ;   ;   ;;;;   ;;;;   ;;;;   ;    
;                                   ;               
;                                ;  ;               
;                                 ;;                

;; produce a pair of tournament winners and cheaters or ERR because there aren't any good maps
(define (manager lop
                 #; [Listof Card]
                 #:cards   (cards (make-list CARDS-PER-GAME 'red))
                 #; [[Listof Destination] -> [Listof Destination]]
                 #:shuffle (shuffle values))
  (define ((run-one-game gm) lop) (referee lop gm #:shuffle shuffle #:cards cards))
  (let/ec return 
    (let*-values
        ([(starters+maps cheaters0) (xmap-send (λ (p) (xsend p start #true)) lop)]
         [(starters      the-maps)  (separate starters+maps)]
         [(the-map)                 (pick-a-map the-maps (min (length starters) MAX-PLAYER-PER-GAME))]
         [(_)                       (when (string? the-map) (return ERR))]
         [(ranking       cheaters1) (run-all-games starters (run-one-game the-map))]
         [(winners       losers)    (if (empty? ranking)
                                        (values '[] '[])
                                        (values (first ranking) (apply append (rest ranking))))]
         [(losers+_      cheaters3) (xmap-send (λ (a) (xsend a end #false)) losers)]
         [(winners+_     cheaters4) (xmap-send (λ (a) (xsend a end #true)) winners)])
      (list (map first winners+_) (append cheaters0 cheaters1 cheaters3 cheaters4)))))

#; {[Listof [List X Y]] -> (values [Listof X] [Listof X])}
(define (separate loxy)
  (values (map first loxy) (map second loxy)))

#; {[Listof GameMap]N  -> GameMap}
(define (pick-a-map all-the-maps player#)
  (cond
    [(empty? all-the-maps) ERR]
    [else
     (define the-game-map (first all-the-maps))
     (define destination* (all-destinations the-game-map))
     (cond 
       [(< (length destination*) (+ (- PICKS-PER DESTS-PER) (* DESTS-PER player#)))
        (pick-a-map (rest all-the-maps) player#)]
       [else the-game-map])]))

;; ---------------------------------------------------------------------------------------------------
#;{[Listof Player] Referee -> (values [Listof Player] [Listof Player])}
;; generative recursion: terminates because either the number of players shrinks per round
;; or the tournament is forcibly stopped because the surviving winners all tie for first place
(define (run-all-games lop0 run-one-game)
  ;; accumulators: previous-winners and cheats 
  (let loop ([lop1 lop0] [previous-winners '()] [cheats '()])
    (define lop  (re-sort lop1 lop0))
    (define lop# (length lop))
    (cond
      ;; not enough for one game 
      [(< lop# MIN-PLAYER-PER-GAME)
       ;; observer
       (values (list lop) cheats)]
      ;; just enough for one game 
      [(<= lop# MAX-PLAYER-PER-GAME)
       (match-define [list ranked new-cheats] (run-one-game lop))
       (values ranked (append new-cheats cheats))]
      [else ;; keep going with rounds of games
       (define games (prepare-games MIN-PLAYER-PER-GAME MAX-PLAYER-PER-GAME lop))
       (match-define `[,winners0 ,new-cheats] (run-one-round-of-games games run-one-game))
       (define winners (apply append winners0))
       (if (equal? winners previous-winners)
           (values winners (append new-cheats cheats))
           (loop winners lop# (append new-cheats cheats)))])))

;; ---------------------------------------------------------------------------------------------------
#; {Player* Referee -> [List Player* Player*]}
(define (run-one-round-of-games games run-one-game)
  (define results  (map run-one-game games))
  (define winners
    (map first
         (filter-map 
          (λ (r)
            (match-define [list ranked _] r)
            (match ranked
              ['[] #f]
              [_ ranked]))
          results)))
  (define cheaters (append-map second results))
  (list winners cheaters))

;; ---------------------------------------------------------------------------------------------------
#;{Player* Player* -> Player*}
;; sort list of winners according to lop0 
(define (re-sort winners lop0)
  (filter (λ (x) (member x winners)) lop0))

(module+ test
  (define box% (class object% [init content] (super-new)))
  (match-define (list box1 box2 box3 box4) (map (λ (x) (new box% [content x])) '(1 2 3 4)))
  (check-equal? (re-sort (list box2 box1 box3) (list box1 box2 box3 box4)) (list box1 box2 box3)))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ examples
  (provide mock+%)
  (define true-mock% (mock%))
  (define (mock+% gm)
    (class true-mock%
      (super-new)
      (define/public (start . x) gm)
      (define/public (end . x) 'thanks))))

(module+ test
  (require SwDev/Debugging/spy)

  (define-values (m1 m2) (values (new (mock+% vtriangle)) (new (mock+% vtriangle))))
  (check-equal? (manager [list m1 m2]) ERR)

  (define-values (k1 k2) (values (new (mock+% vrectangle)) (new (mock+% vrectangle))))
  (check-equal? (manager [list k1 k2]) `[ () [,k2 ,k1]]))
