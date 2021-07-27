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
  [silly-strategy%
   (class/c
    (init-field (the-game-map game-map?))
    (pick-destinations (->m (list/c any/c any/c any/c any/c any/c) (list/c any/c any/c any/c)))
    (choose-action (->m pstate? (or/c 'more-cards (list/c set? any/c seg#?)))))]))

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
(require Trains/Common/basic-constants)

(module+ test
  (require (submod ".."))
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

(define silly-strategy%
  (class object%
    (init-field the-game-map)

    (field
     [destination1 #false]
     [destination2 #false])

    #; {type Destination = [List City City] : symbol<? holds for the 2 cities}
    #; {Graph [List Destination Destination Destination Destination Destination]
              ->
              [List Destination Destination]}
    ;; lexicographic ordering, by symbol<?, of destinations: take 2 
    (define/public (pick-destinations five-destinations)
      (define chosen (take (sort five-destinations lexi<?) 2))
      (set!-values (destination1 destination2) (apply values chosen))
      (remove* chosen five-destinations))

    #; {type Action = (U 'more-cards Connection)}
    #; {PlayerState -> Action}
    (define/public (choose-action ps)
      (match-define [pstate I others] ps)
      (match-define [ii _d1 _d2 _r cards _acquired] I)
      (cond
        [(< (total-number-of-cards cards) 10) 'more-cards]
        [else
         (define available  (set->list (all-available-connections the-game-map ps)))
         (define acquirable (filter (can-acquire? cards) available))
         (if (empty? acquirable)
             'more-cards
             (first (sort acquirable lexi->length->color<?)))]))

    #; {Color Color -> Void}
    (define/public (more-cards c1 c2)
      (void))

    (super-new)))

#; {[Hash Color N] -> Connection -> Boolean}
(define ((can-acquire? cards) x)
  (match-define [list cities color seg#] x)
  (>= (hash-ref cards color 0) seg#))

#; {[Setof City] -> [Listof City] : ordered by symbol<?}
(define (->list s)
  (sort (set->list s) symbol<?))

#; {[Hash Color N] -> N}
(define (total-number-of-cards c)
  (apply + (map cdr (hash->list c))))

#; {Connection Connection -> Boolean}
(define (lexi->length->color<? c1 c2)
  (define l1 (->list (first c1)))
  (define l2 (->list (first c2)))
  (or (lexi<? l1 l2)
      (and (equal? l1 l2)
           (or (< (third c1) (third c2))
               (and (= (third c1) (third c2))
                    (symbol<? (second c1) (second c2)))))))

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

(module+ test
  (define strat (new silly-strategy% [the-game-map vtriangle]))

  (check-equal?
   (send strat pick-destinations '[(Boston Seattle)
                                   (Boston Seattle)
                                   (Boston Orlando)
                                   (Boston Orlando)
                                   (Orlando Seattle)])
   '[(Boston Seattle) (Boston Seattle) (Orlando Seattle)])
   

  (check-equal? (get-field destination1 strat) '(Boston Orlando))
  (check-equal? (get-field destination2 strat) '(Boston Orlando))

  (check-equal? (send strat choose-action pstate1) 'more-cards)
  (check-equal? (send strat choose-action pstate2) `[,(set 'Boston 'Orlando) green 5])
  (check-equal? (send strat more-cards 'green 'red) (void)))
