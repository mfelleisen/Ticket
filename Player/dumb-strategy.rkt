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

(provide (contract-out [dumb-strategy% strategy/c%])
         (rename-out [dumb-strategy% strategy%]))
                       

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

(define dumb-strategy%
  (class base-strategy%
    
    (inherit-field the-game-map rails# cards destination1 destination2)

    #; {type Destination = [List City City] : symbol<? holds for the 2 cities}
    #; {Graph [List Destination Destination Destination Destination Destination]
              ->
              [List Destination Destination Destination]}
    ;; take the first 2 of the sorted destinations
    (define/augment (pick-destinations sorted-destinations)
      (take (reverse sorted-destinations) DESTS-PER))

    #; {[Listof Cards] N -> (values Boolean Action)}
    (define/override (stop-here cards rails#)
      (values #false MORE))

    (super-new)))

(define cheats-strategy%
  (class dumb-strategy%

    (super-new)

    (inherit-field the-game-map rails# cards destination1 destination2)

    (define/override (stop-here cards rails#)
      (values #true (make-bad-connection the-game-map)))))

(define (make-bad-connection the-game-map)
  `[A B red 3])


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
  (define strat-tri (new dumb-strategy% [the-game-map vtriangle] [rails# 45]))
  (define strat-rec (new dumb-strategy% [the-game-map vrectangle][rails# 45]))

  (check-equal? (send strat-tri pick-destinations destinations)
                (apply set (take (reverse destinations-lst) 3)))
  
  (check-equal? (get-field destination1 strat-tri) '(Orlando Seattle))
  (check-equal? (get-field destination2 strat-tri) '(Boston Seattle))

  (check-equal? (send strat-tri choose-action pstate1) '(Boston Orlando green 5))
  (check-equal? (send strat-tri choose-action pstate2) `[Boston Orlando green 5])
  (check-equal? (send strat-tri choose-action (like-pstate2 'green 3)) MORE)
  (check-equal? (send strat-tri more-cards 'green 'red) (void))
  (check-equal? (send strat-rec choose-action (like-pstate2 'green 2)) '(Orlando |San Diego| blue 5)))
