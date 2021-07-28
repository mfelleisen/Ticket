#lang racket

;; representation of a player's knowledge about the game

(provide

 #; {type Connection  = [list (Set City City) Color Length]}
 ;; a connection between two cities has a color and a length 
 
 #; {Map PlayerState -> [Setof Connections]}
 all-available-connections

 (struct-out pstate)
 (struct-out ii))

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

(require Trains/Common/map)

(module+ examples
  (provide pstate1 pstate2 c0 c1))

(module+ test
  (require (submod ".." examples))
  (require (submod ".."))
  (require (submod Trains/Common/map examples))
  (require rackunit))
           

;                                                                          
;                                                                          
;        ;                                                                 
;        ;            ;                                                    
;        ;            ;                                                    
;    ;;; ;    ;;;   ;;;;;;    ;;;            ;;;;;   ;;;;   ; ;;;          
;    ;  ;;   ;   ;    ;      ;   ;           ;;      ;  ;;  ;;  ;          
;   ;    ;       ;    ;          ;           ;      ;    ;  ;    ;         
;   ;    ;   ;;;;;    ;      ;;;;;           ;      ;;;;;;  ;    ;         
;   ;    ;  ;    ;    ;     ;    ;           ;      ;       ;    ;         
;   ;    ;  ;    ;    ;     ;    ;           ;      ;       ;    ;    ;;   
;    ;  ;;  ;   ;;    ;     ;   ;;           ;       ;      ;;  ;     ;;   
;    ;;; ;   ;;; ;     ;;;   ;;; ;           ;       ;;;;;  ; ;;;     ;;   
;                                                           ;              
;                                                           ;              
;                                                           ;              
;                                                                          

(struct pstate [I others] #:transparent)
(struct ii [destination1 destination2 rails cards connections] #:transparent)

#; {type PlayerState = (pstate MePlayer [Listof Player])}
;; what the player knows about itself and others 

#; {type MePlayer    = (ii Desitination Destination Natural [Hash Color Natural] Player)}
;; the two destination cards, the rails left, the colored cards, and this player's possessions

#; {type Player      = [Setof Connection]}

#; {type Destination = [set City City]} 
;; a destination card specifies two cities; there is guaranteed to be a path between them

(module+ examples
  (define cards1  (hash 'green 5))
  (define c0 (set [list (set 'Orlando 'Seattle) 'blue 5]))
  (define (ii- cards1) (ii (set 'Boston 'Seattle) (set 'Boston 'Orlando) 40 cards1 c0))
  (define c1 (set [list (set 'Boston 'Seattle) 'red 3]))
  (define pstate1 (pstate (ii- cards1) (list c1)))

  (define cards2  (hash 'green 5 'blue 7 'red 2))
  (define pstate2 (pstate (ii- cards2) (list c1))))


;                                                                          
;                                                                          
;      ;;;                                     ;                           
;     ;                               ;        ;                           
;     ;                               ;                                    
;   ;;;;;;  ;    ;  ; ;;;     ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;    ;;;;  
;     ;     ;    ;  ;;   ;   ;   ;    ;        ;    ;;  ;;  ;;   ;  ;    ; 
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;      
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;;;    
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;     ;;; 
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;       ; 
;     ;     ;   ;;  ;    ;   ;   ;    ;        ;    ;;  ;;  ;    ;  ;    ; 
;     ;      ;;; ;  ;    ;    ;;;      ;;;  ;;;;;;;  ;;;;   ;    ;   ;;;;  
;                                                                          
;                                                                          
;                                                                          
;

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
  (for/sum ([c connections]) (third c)))

;; given my state and a map, can I still connect a destination

;; given a state, how close is the game from being over

;; serialize the player state 

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
   (set-subtract (game-map-all-connections vtriangle) c0 c1)))
