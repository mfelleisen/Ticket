#lang racket

;; a silly strategy of choosing destinations and connections by name 

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

(require (only-in Trains/Player/astrategy strategy/c%))

(provide (contract-out [simple-strategy% strategy/c%])
         (rename-out [simple-strategy% strategy%]))

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

(require (except-in Trains/Player/astrategy strategy/c%))
(require Trains/Common/basic-constants)
(require Trains/Common/player-interface)



(module+ test
  (require (submod ".."))
  (require (submod Trains/Player/astrategy examples))
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples))
  (require Trains/Common/player-interface)
  (require rackunit))

;                                                                  
;                                                                  
;                                                                  
;             ;                       ;                            
;             ;                       ;                            
;    ;;;;   ;;;;;;   ;;;;;    ;;;   ;;;;;;   ;;;;    ;;; ;  ;    ; 
;   ;    ;    ;      ;;      ;   ;    ;      ;  ;;   ;  ;;   ;   ; 
;   ;         ;      ;           ;    ;     ;    ;  ;    ;   ;  ;  
;   ;;;       ;      ;       ;;;;;    ;     ;;;;;;  ;    ;   ;  ;  
;      ;;;    ;      ;      ;    ;    ;     ;       ;    ;    ; ;  
;        ;    ;      ;      ;    ;    ;     ;       ;    ;    ;;   
;   ;    ;    ;      ;      ;   ;;    ;      ;       ;  ;;    ;;   
;    ;;;;      ;;;   ;       ;;; ;     ;;;   ;;;;;   ;;; ;     ;   
;                                                        ;     ;   
;                                                    ;  ;;    ;    
;                                                     ;;;    ;;    
;                                                                  

(define simple-strategy%
  (class base-strategy%
    
    (inherit-field the-game-map rails# cards destination1 destination2)

    #; {type Destination = [List City City] : symbol<? holds for the 2 cities}
    #; {Graph [List Destination Destination Destination Destination Destination]
              ->
              [List Destination Destination Destination]}
    ;; take the first 2 of the sorted destinations
    (define/augment (pick-destinations sorted-destinations0)
      (take sorted-destinations0 DESTS-PER))

    #; {[Listof Cards] N -> (values Boolean Action)}
    ;; buy if you have more than 10 cards, otherwise ask for MORE cards 
    (define/override (stop-here cards rails#)
      (values (< (total-number-of-cards cards) 10) MORE))

    (super-new)))

#; {[Hash Color N] -> N}
(define (total-number-of-cards c)
  (apply + (map cdr (hash->list c))))

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
  (define strat-tri   (new simple-strategy% [the-game-map vtriangle] [rails# 45]))
  (define strat-tri++ (new simple-strategy% [the-game-map vtriangle++] [rails# 45]))
  (define strat-rec   (new simple-strategy% [the-game-map vrectangle][rails# 45]))

  (check-equal? (send strat-tri pick-destinations destinations) (apply set (take destinations-lst 3)))
  
  (check-equal? (get-field destination1 strat-tri) '(Boston Chicago))
  (check-equal? (get-field destination2 strat-tri) '(Boston Orlando))

  (check-equal? (send strat-tri choose-action pstate1) MORE)
  (check-equal? (send strat-tri choose-action pstate2) `[Boston Orlando green 5])
  (check-equal? (send strat-tri choose-action (like-pstate2 'green 3)) MORE)
  (check-equal? (send strat-tri more-cards 'green 'red) (void))
  (check-equal? (send strat-rec choose-action (like-pstate2 'green 2)) '(Orlando |San Diego| blue 5))

  (define okay '[Boston Orlando green 5])
  (check-equal? (send strat-tri choose-action pstate-play) okay)
  (check-equal? (send strat-tri++ choose-action pstate-play) okay)
  (check-equal? (send strat-tri++ choose-action pstate-play+) okay) "destinations not connected !!!")
