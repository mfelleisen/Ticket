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
    (pick-destinations
     (->m (list/c any/c any/c any/c any/c any/c) (list/c any/c any/c any/c)))
    (choose-action
     (->m pstate? (or/c string? (list/c symbol? symbol? color? seg#?)))))]))

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

(define MORE "more cards")

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

    #; {type Action = (U MORE Connection)}
    #; {PlayerState -> Action}
    (define/public (choose-action ps)
      (match-define [pstate I others] ps)
      (match-define [ii _d1 _d2 _r cards _acquired] I)
      (cond
        [(< (total-number-of-cards cards) 10) MORE]
        [else
         (define available  (set->list (all-available-connections the-game-map ps)))
         (define acquirable (filter (can-acquire? cards) available))
         (if (empty? acquirable)
             MORE
             (first (sort acquirable lexi->length->color<?)))]))

    #; {Color Color -> Void}
    (define/public (more-cards c1 c2)
      (void))

    (super-new)))

#; {[Hash Color N] -> Connection -> Boolean}
(define ((can-acquire? cards) x)
  (match-define [list _c _d color seg#] x)
  (>= (hash-ref cards color 0) seg#))

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

(module+ test
  (define strat-tri (new silly-strategy% [the-game-map vtriangle]))
  (define strat-rec (new silly-strategy% [the-game-map vrectangle]))

  (check-equal?
   (send strat-tri pick-destinations '[(Boston Seattle)
                                   (Boston Seattle)
                                   (Boston Orlando)
                                   (Boston Orlando)
                                   (Orlando Seattle)])
   '[(Boston Seattle) (Boston Seattle) (Orlando Seattle)])
  
  (check-equal? (get-field destination1 strat-tri) '(Boston Orlando))
  (check-equal? (get-field destination2 strat-tri) '(Boston Orlando))

  (check-equal? (send strat-tri choose-action pstate1) MORE)
  (check-equal? (send strat-tri choose-action pstate2) `[Boston Orlando green 5])
  (check-equal? (send strat-tri choose-action (like-pstate2 'green 3)) MORE)
  (check-equal? (send strat-tri more-cards 'green 'red) (void))
  (check-equal? (send strat-rec choose-action (like-pstate2 'green 2)) '(Orlando |San Diego| blue 5)))
