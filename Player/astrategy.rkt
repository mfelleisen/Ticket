#lang racket

;; an abstract module for strategies, contains the common elements 

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

(require Trains/Player/istrategy)

(provide
 strategy/c%

 (contract-out
  [base-strategy% strategy/c%])

 #; {N [Hash Color N] -> Connection -> Boolean}
 can-acquire?
 
 #; {Connection Connection -> Boolean}
 lexi->length->color<?

 #; {City City -> Boolean}
 lexi<?)

(module+ examples
  (provide destinations destinations-lst))

(module+ homework
  (provide MORE))

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
(require Trains/Common/state)
(require Trains/Common/player-interface)

;                                                                                  
;                                                                                  
;           ;                                                                      
;           ;                 ;                               ;                    
;           ;                 ;                               ;                    
;     ;;;   ; ;;;    ;;;;   ;;;;;;   ;;;;;    ;;;     ;;;   ;;;;;;            ;;;  
;    ;   ;  ;;  ;   ;    ;    ;      ;;      ;   ;   ;   ;    ;              ;   ; 
;        ;  ;    ;  ;         ;      ;           ;  ;         ;             ;      
;    ;;;;;  ;    ;  ;;;       ;      ;       ;;;;;  ;         ;             ;      
;   ;    ;  ;    ;     ;;;    ;      ;      ;    ;  ;         ;             ;      
;   ;    ;  ;    ;       ;    ;      ;      ;    ;  ;         ;             ;      
;   ;   ;;  ;;  ;   ;    ;    ;      ;      ;   ;;   ;   ;    ;              ;   ; 
;    ;;; ;  ; ;;;    ;;;;      ;;;   ;       ;;; ;    ;;;      ;;;            ;;;  
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  

(define base-strategy%
  (class object%
    (init-field the-game-map rails# [cards '()])
    
    (field
     [destination1 #false]
     [destination2 #false])

    #; {type Destination = [List City City] : symbol<? holds for the 2 cities}
    #; {Graph [Set Destination Destination Destination Destination Destination]
              ->
              [Set Destination Destination Destination]}
    ;; lexicographic ordering, by symbol<?, of destinations:
    ;; -- sort and let `inner` decide which ones to pick 
    (define/pubment (pick-destinations five-destinations0)
      (define five-destinations (set->list five-destinations0))
      (define sorted-destinations (sort five-destinations lexi<?))
      (define chosen (inner '[] pick-destinations sorted-destinations))
      (set!-values (destination1 destination2) (apply values chosen))
      (apply set (remove* chosen five-destinations)))

    #; {type Action = (U MORE Connection)}
    #; {PlayerState -> Action}
    (define/public (choose-action ps)
      (define I (pstate-I ps))
      (define cards  (ii-cards I))
      (define rails# (ii-rails I))
      
      (define-values (stop? result) (stop-here cards rails#))

      (cond
        [stop? result]
        [else
         (define available  (set->list (all-available-connections the-game-map ps)))
         (define acquirable (filter (can-acquire? rails# cards) available))
         (if (empty? acquirable)
             MORE
             (first (sort acquirable lexi->length->color<?)))]))

    #; {[Listof Cards] N -> (values Boolean Action)}
    (define/public (stop-here cards rails#)
      (values #false MORE))

    #; {Color Color -> Void}
    (define/public (more-cards c1 c2)
      (set! cards (list* c1 c2 cards)))

    (super-new)))

;                                  
;                                  
;                                  
;                                  
;                                  
;     ;;;   ;    ;  ;   ;;         
;    ;   ;  ;    ;   ;  ;          
;        ;  ;    ;    ;;           
;    ;;;;;  ;    ;    ;;           
;   ;    ;  ;    ;    ;;           
;   ;    ;  ;    ;    ;;      ;;   
;   ;   ;;  ;   ;;   ;  ;     ;;   
;    ;;; ;   ;;; ;  ;    ;    ;;   
;                                  
;                                  
;                                  
;                                  

(define ((can-acquire? rails# cards) x)
  (match-define [list _c _d color seg#] x)
  (and (>= rails# seg#) (>= (hash-ref cards color 0) seg#)))

(define (lexi->length->color<? c1 c2)
  (let ((from-to-1 (take c1 2))
        (from-to-2 (take c2 2)))
    (or (lexi<? from-to-1 from-to-2)
        (and (equal? from-to-1 from-to-2)
             (let ([length-1 (connection-seg# c1)]
                   [length-2 (connection-seg# c2)])
               (or (< length-1 length-2)
                   (and (= length-1 length-2)
                        (symbol<? (connection-color c1) (connection-color c2)))))))))

(define (lexi<? d1 d2)
  (or (symbol<? (first d1) (first d2))
      (and (symbol=? (first d1) (first d2))
           (symbol<? (second d1) (second d2)))))


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

(module+ examples
  (define destinations-lst
    '[(Orlando Seattle)
      (Boston Seattle)
      (Boston SanDiego)
      (Boston Chicago)
      (Boston Orlando)])
  (define destinations
    (apply set destinations-lst)))