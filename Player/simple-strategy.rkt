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

(provide
 (contract-out 
  [simple-strategy%
   (class/c
    (init-field (the-game-map game-map?) (rails# natural?))
    (pick-destinations (->m (set/c any/c) (set/c any/c)))
    (choose-action     (->m pstate? (or/c string? action?))))]))

(module+ examples
  (provide destinations destinations-list))

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

(require Trains/Common/basic-constants)
(require Trains/Common/map)
(require Trains/Common/state)
(require Trains/Common/player-interface)


(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples))
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
  (class object%
    (init-field the-game-map rails#)

    (field
     [destination1 #false]
     [destination2 #false])

    #; {type Destination = [List City City] : symbol<? holds for the 2 cities}
    #; {Graph [Set Destination Destination Destination Destination Destination]
              ->
              [Set Destination Destination Destination]}
    ;; lexicographic ordering, by symbol<?, of destinations: take 2 
    (define/public (pick-destinations five-destinations0)
      (define five-destinations (set->list five-destinations0))
      (define chosen (take (sort five-destinations lexi<?) DESTS-PER))
      (set!-values (destination1 destination2) (apply values chosen))
      (apply set (remove* chosen five-destinations)))

    #; {type Action = (U MORE Connection)}
    #; {PlayerState -> Action}
    (define/public (choose-action ps)
      (match-define [pstate I others] ps)
      (define cards  (ii-cards I))
      (define rails# (ii-rails I))
      (cond
        [(< (total-number-of-cards cards) 10) MORE]
        [else
         (define available  (set->list (all-available-connections the-game-map ps)))
         (define acquirable (filter (can-acquire? rails# cards) available))
         (if (empty? acquirable)
             MORE
             (first (sort acquirable lexi->length->color<?)))]))

    #; {Color Color -> Void}
    (define/public (more-cards c1 c2)
      (void))

    (super-new)))

#; {N [Hash Color N] -> Connection -> Boolean}
(define ((can-acquire? rails# cards) x)
  (match-define [list _c _d color seg#] x)
  (and (>= rails# seg#) (>= (hash-ref cards color 0) seg#)))

#; {[Hash Color N] -> N}
(define (total-number-of-cards c)
  (apply + (map cdr (hash->list c))))

#; {Connection Connection -> Boolean}
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

#; {City City -> Boolean}
(define (lexi<? d1 d2)
  (or (symbol<? (first d1) (first d2))
      (and (symbol=? (first d1) (first d2))
           (symbol<? (second d1) (second d2)))))

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
  (define destinations-list
    '[(Orlando Seattle)
      (Boston Seattle)
      (Boston SanDiego)
      (Boston Chicago)
      (Boston Orlando)])
  (define destinations
    (apply set destinations-list)))

(module+ test
  (define strat-tri (new simple-strategy% [the-game-map vtriangle] [rails# 45]))
  (define strat-rec (new simple-strategy% [the-game-map vrectangle][rails# 45]))

  (check-equal? (send strat-tri pick-destinations destinations) (apply set (take destinations-list 3)))
  
  (check-equal? (get-field destination1 strat-tri) '(Boston Chicago))
  (check-equal? (get-field destination2 strat-tri) '(Boston Orlando))

  (check-equal? (send strat-tri choose-action pstate1) MORE)
  (check-equal? (send strat-tri choose-action pstate2) `[Boston Orlando green 5])
  (check-equal? (send strat-tri choose-action (like-pstate2 'green 3)) MORE)
  (check-equal? (send strat-tri more-cards 'green 'red) (void))
  (check-equal? (send strat-rec choose-action (like-pstate2 'green 2)) '(Orlando |San Diego| blue 5)))
