#lang racket

;; representation of a player's knowledge about the game

(provide

 #; {type Connection  = [list City City Color Length]}
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
  (provide pstate1 pstate2
           #; {Color N -> PlayerState : like pstate2, different color count for c}
           like-pstate2 
           c0 c1))

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
(struct ii [destination1 destination2 rails cards connections{ payload #:mutable}] #:transparent)

#; {type PlayerState  = (pstate [MePlayer Any] [Listof Player])}
;; what the player knows about itself and others 

#; {type [MePlayer X] = (ii Desitination Destination Natural [Hash Color Natural] Player X)}
;; the two destination cards, the rails left, the colored cards, and this player's possessions

#; {type Player       = [Setof Connection]}

#; {type Destination  = [List City City]} 
;; a destination card specifies two cities; there is guaranteed to be a path between them

(module+ examples
  (define cards1  (hash 'green 5))
  (define c0 (set [list 'Orlando 'Seattle 'blue 5]))
  (define (ii- cards1) (ii (list 'Boston 'Seattle) (list 'Boston 'Orlando) 40 cards1 c0 #f))

  (define c1 (set [list 'Boston 'Seattle 'red 3]))
  (define pstate1 (pstate (ii- cards1) (list c1)))

  (define cards2  (hash 'green 5 'blue 7 'red 6))
  
  (define (like-pstate2 c n)
    (pstate (ii- (hash-set cards2 c n)) (list c1)))

  (define pstate2 (like-pstate2 'green 5)))


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
  (for/sum ([c connections]) (connection-seg# c)))

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
