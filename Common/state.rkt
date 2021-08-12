#lang racket

;; representation of a player's knowledge about the game

(require Trains/Common/map)

(provide

 #; {PlayerState Connection -> Player}
 ;; ASSUME the action is legal
 ii-acquire

 #; {PlayerState [Listof Color] -> PlayerState}
 ii+cards

 #; {Player -> Boolean}
 ii-final?

 #; {MePlayer Path -> Boolean}
 ii-path-covered?

 #; {MePlayer -> N}
 ii-conn-score

 #; {PlayerState [Listof Path] -> Integer}
 ii-destinations-connected 

 #; {type Connection  = [list City City Color Length]}
 ;; a connection between two cities has a color and a length

 #; {Map PlayerState -> [Setof Connection]}
 all-available-connections

 #; {PlayerState [Listof Connection] Connection -> Boolean}
 legal-action?

 #; {MePlayer[False] X -> MePlayer[X]}
 ii+payload
 
 ii? 
 (contract-out
  [ii (-> destination/c destination/c natural? hash? [set/c connection/c] any/c ii?)])

 ii-destination1
 ii-destination2
 ii-rails
 ii-cards
 ii-connections
 ii-payload
 ii-payload--
 
 pstate?
 (contract-out
  (pstate (-> ii? [listof [set/c connection/c]] pstate?)))
 pstate-I
 pstate-others)

(module+ examples
  (provide pstate1 pstate2 
           #; {Color N -> PlayerState : like pstate2, different color count for c}
           like-pstate2 
           conns0 conns1))

;                                                                                                  
;                                                                                                  
;        ;                                       ;                             ;                   
;        ;                                       ;                             ;                   
;        ;                                       ;                                                 
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ; ;;;    ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;  
;    ;  ;;   ;  ;;  ;;  ;    ;  ;;  ;;   ;   ;  ;;   ;  ;;  ;;   ;   ;   ;     ;     ;  ;;  ;    ; 
;   ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;          ;    ;    ;  ;      
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;    ;  ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;;;    
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;          ;;; 
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;            ; 
;    ;  ;;   ;      ;;  ;    ;      ;    ;   ;  ;;   ;      ;    ;   ;   ;     ;     ;      ;    ; 
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;    ;   ;;; ;   ;;;;;  ;    ;    ;;;   ;;;;;;;  ;;;;;   ;;;;  
;                   ;                                                                              
;                   ;                                                                              
;                   ;                                                                              
;                                                                                                  

(require Trains/Common/basic-constants)
(require Trains/Common/map)

(module+ test
  (require (submod ".." examples))
  (require (submod ".."))
  (require (submod Trains/Common/map examples))
  (require rackunit))

;                                                                          
;                                                                          
;                                    ;;;                                   
;                                      ;                                   
;                                      ;                                   
;   ;;;;;;   ;;;;           ; ;;;      ;      ;;;   ;    ;   ;;;;    ;;;;  
;   ;  ;  ; ;    ;          ;;  ;;     ;     ;   ;   ;  ;;  ;    ;   ;;  ; 
;   ;  ;  ; ;;;;;;          ;    ;     ;         ;   ;  ;   ;;;;;;   ;     
;   ;  ;  ; ;               ;    ;     ;     ;;;;;   ;  ;   ;        ;     
;   ;  ;  ; ;               ;    ;     ;    ;    ;    ; ;   ;        ;     
;   ;  ;  ; ;;   ;          ;;  ;;     ;    ;   ;;    ;;    ;;   ;   ;     
;   ;  ;  ;  ;;;;;          ; ;;;       ;;;  ;;; ;     ;     ;;;;;   ;     
;                           ;                          ;                   
;                           ;                         ;                    
;                           ;                        ;;                    
;                                                                          

(struct ii [destination1 destination2 rails cards connections payload] #:transparent)
#; {type [MePlayer X] = (ii Desitination Destination Natural [Hash Color Natural] Player X)}
;; the two destination cards, the rails left, the colored cards, and this player's possessions

(define (ii-payload-- ii-player)
  (struct-copy ii ii-player [payload #false]))

(define (ii-acquire ii-player c)
  (define seg#  (connection-seg# c))
  (define color (connection-color c))
  (match-define (ii d-1 d-2 rails-left cards connections xplayer) ii-player)
  (ii d-1 d-2 (- rails-left seg#) (-cards cards color seg#) (set-add connections c) xplayer))

#; {[Hash Color N] Color N -> [Hash Color N]}
(define (-cards cards0 color n)
  (if (= (hash-ref cards0 color 0) n)
      (hash-remove cards0 color)
      (hash-update cards0 color (λ (old) (- old n)))))

(define (ii+cards ii-player new-cards)
  (match-define (ii d-1 d-2 rails-left cards0 connections xplayer) ii-player)
  (define cards1 (for/fold ([cards cards0]) ([c new-cards]) (hash-update cards c add1 0)))
  (ii d-1 d-2 rails-left cards1 connections xplayer))

(define (ii-final? ii-player)
  (< (ii-rails ii-player) RAILS-MIN))

(define (ii-path-covered? ii-player path)
  (match-define (ii d-1 d-2 rails-left cards0 connections xplayer) ii-player)
  (covered? path connections))

(define (ii-conn-score ii-player)
  (match-define (ii d-1 d-2 rails-left cards0 connections xplayer) ii-player)
  (for/sum ([c connections]) (connection-seg# c)))

(define (ii-destinations-connected ii-player map)
  (match-define (ii d-1 d-2 rails-left cards0 connections xplayer) ii-player)
  (define (plus-minus-points dest)
    (define from (first dest))
    (define to   (second dest))
    (define any-path-connection
      (for/or ([p (all-paths map from to)])
        ;; MF: this is suspicious: it should be symmetric but switching the next two kills a test case
        (define originations (take (first p) 2))
        (define destinations (take (last p) 2))
        (and (member from originations) (member to destinations) (covered? p connections))))
    (if any-path-connection POINTS-PER (- POINTS-PER)))
  (+ (plus-minus-points d-1) (plus-minus-points d-2)))

(define (ii+payload ii-player pl)
  (when (ii-payload ii-player) (error 'ii+payload "payload already exists ~e" (ii-payload ii-player)))
  (struct-copy ii ii-player [payload pl]))

#; [Path [Setof Connection] -> Boolean]
(define (covered? path connections)
  (for/and ([c path]) (set-member? connections c)))

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

(module+ examples
  (provide ii- cards1 cards2 ii-final ii-play path01)
  
  (define cards1 (hash 'green 5))
  (define cards2 (hash 'green 5 'blue 7 'red 6))

  (define orl-sea '[Orlando Seattle blue 5])
  (define bos-sea '[Boston Seattle red 3])
  (define conns0 (set orl-sea))
  (define conns1 (set bos-sea))
  (define path01 (list bos-sea orl-sea))
  
  (define (ii- cards1) (ii '(Boston Seattle) '(Boston Orlando) 40 cards1 conns0 #f))
  (define (ii-r r (cards2 cards2) (conns0 conns0))
    (ii '(Boston Seattle) '(Boston Orlando) r cards2 conns0 #f))
  
  (define ii-final (ii-r (- RAILS-MIN 1) (-cards cards2 'red 3) (set-union conns0 conns1)))
  (define ii-play  (ii-r (+ RAILS-MIN 2))))

(module+ test
  
  (check-equal? (ii-destinations-connected ii-final vtriangle) (* 2 POINTS-PER))
  (check-equal? (ii-destinations-connected ii-play vtriangle) (* -2 POINTS-PER))

  (check-true (ii-final? ii-final))
  (check-false (ii-final? ii-play))

  (define blues (make-list 7 'blue))
  (check-equal? (ii+cards (ii+cards (ii- cards1) blues) (make-list 6 'red)) (ii- cards2))

  (check-equal? (ii-acquire ii-play '[Boston Seattle red 3]) ii-final)

  (check-true (ii-path-covered? (ii-acquire ii-play '[Boston Seattle red 3]) path01))
  (check-false (ii-path-covered? ii-play path01))

  (check-equal? (ii-conn-score ii-play) 5))

;                                                  
;                                                  
;                                                  
;                     ;               ;            
;                     ;               ;            
;   ; ;;;    ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;  
;   ;;  ;;  ;    ;    ;      ;   ;    ;     ;    ; 
;   ;    ;  ;         ;          ;    ;     ;;;;;; 
;   ;    ;   ;;;;     ;      ;;;;;    ;     ;      
;   ;    ;       ;    ;     ;    ;    ;     ;      
;   ;;  ;;  ;    ;    ;     ;   ;;    ;     ;;   ; 
;   ; ;;;    ;;;;      ;;;   ;;; ;     ;;;   ;;;;; 
;   ;                                              
;   ;                                              
;   ;                                              
;                                                  

(struct pstate [I others] #:transparent)
#; {type PlayerState  = (pstate [MePlayer Any] [Listof Player])}
#; {type Player       = [Setof Connection]}
;; what the player knows about itself and others 

(module+ examples
  
  (define pstate1 (pstate (ii- cards1) (list conns1)))
  
  (define (like-pstate2 c n) (pstate (ii- (hash-set cards2 c n)) (list conns1)))

  (define pstate2 (like-pstate2 'green 5))
  (define pstate3 (pstate ii-final (list conns1))))


#; {Map PlayerState -> [Setof Connections]}
;; determine the connections the active player can still acquire 
(define (all-available-connections m ps)
  (define total (game-map-all-connections m))
  (define other (apply set-union (pstate-others ps)))
  (set-subtract total other (ii-connections (pstate-I ps))))

(define TERMINATION# 3)

#; {PlayerState N -> N}
(define (termination ps rails0)
  (match-define [pstate I others] ps)
  (define my-acquisitions (ii-connections I))
  (define rails-consumed  (map rails-spent (cons my-acquisitions others)))
  (- rails0 (apply max rails-consumed)))

(define (rails-spent connections)
  (for/sum ([c connections]) (connection-seg# c)))

#; {PlayerState [Setof Connections] Connection -> Boolean}
;; can this player acquire the specified connection 
(define (legal-action? ps total c)
  (define active (pstate-I ps))
  (define other  (apply set-union (ii-connections active) (pstate-others ps)))
  (define avail  (set-subtract total other))
  (cond
    [(not (set-member? avail c)) #false]
    [else
     (define seg# (connection-seg# c))
     (and (>= (ii-rails active) seg#)
          (>= (hash-ref (ii-cards active) (connection-color c) 0) seg#))]))

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

  (check-equal? (termination pstate1 45) 40)
  (check-equal? (termination pstate1 6) 1)
  (check-equal? (termination pstate1 8) 3)
  
 
  (check-equal? 
   (all-available-connections vtriangle pstate1)
   (set-subtract (game-map-all-connections vtriangle) conns0 conns1))
  
  (define total (game-map-all-connections vtriangle))
  
  (check-false (legal-action? pstate1 total (list 'Boston 'Seattle 'red  3)))
  (check-true (legal-action? pstate1 total (list 'Boston 'Orlando 'green  5))))
