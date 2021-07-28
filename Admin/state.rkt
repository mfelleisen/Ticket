#lang racket

;; representation of a referee's knowledge about the game

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

(require Trains/Common/state)
(require Trains/Common/map)

(module+ examples
  (provide rstate1 ii1 ii2))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Trains/Common/state examples))
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

#; {type RefereeState = [Listof MePlayer]}

(module+ examples
  (define cards1 (hash 'green 5))
  (define dest1  (set 'Boston 'Seattle))
  (define ii1 (ii dest1 (set 'Boston 'Orlando) 40 cards1 (set)))
  (define ii2 (ii dest1 (set 'Seattle 'Orlando) 5 cards1 (set [list (set 'Boston 'Seattle) 'red 3])))

  (define rstate1 (list ii1 ii2)))

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

#; {RefereeState -> PlayerState}
(define (rstate->pstate rs)
  (pstate (first rs) (map ii-connections (rest rs))))

#; {Map PlaterState Connection -> Boolean}
;; can this player acquire the specified connection 
(define (legal-action? m rs c)
  (define total (game-map-all-connections m))
  (define active (first rs))
  (define other (apply set-union (map ii-connections rs)))
  (define avail (set-subtract total other))
  (cond
    [(not (set-member? avail c)) #false]
    [else (>= (hash-ref (ii-cards active) (second c) 0) (third c))]))


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

  (check-equal? (rstate->pstate rstate1) (pstate ii1 (list (ii-connections ii2))))

  (check-false (legal-action? vtriangle rstate1 (list (set 'Boston 'Seattle) 'red  3)))
  (check-true (legal-action? vtriangle rstate1 (list (set 'Boston 'Orlando) 'green  5))))
