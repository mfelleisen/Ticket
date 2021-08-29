#lang racket

;; the Cheat strategy of milestone 8

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

(provide (contract-out [cheat-strategy% strategy/c%])
         (rename-out [cheat-strategy% strategy%]))

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
(require Trains/Player/simple-strategy)
(require SwDev/Lib/should-be-racket)

(module+ test
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples))
  (require rackunit))

;                                          
;                                          
;           ;                              
;           ;                         ;    
;           ;                         ;    
;     ;;;   ; ;;;    ;;;;     ;;;   ;;;;;; 
;    ;   ;  ;;   ;   ;  ;;   ;   ;    ;    
;   ;       ;    ;  ;    ;       ;    ;    
;   ;       ;    ;  ;;;;;;   ;;;;;    ;    
;   ;       ;    ;  ;       ;    ;    ;    
;   ;       ;    ;  ;       ;    ;    ;    
;    ;   ;  ;    ;   ;      ;   ;;    ;    
;     ;;;   ;    ;   ;;;;;   ;;; ;     ;;; 
;                                          
;                                          
;                                          
;                                          

(define cheat-strategy%
  (class simple-strategy%
    (super-new)

    (inherit-field the-game-map)

    (define/override (choose-action ps)
      (define conns  (set->list (game-map-all-connections the-game-map)))
      (define cities (game-map-cities the-game-map))
      
      (define under-connected-cities 
        (for*/first ([c cities]
                     [d cities]
                     [d-c-conns (in-value  (filter (λ (x) (equal? (take x 2) (list c d))) conns))]
                     #:when (and (symbol<? c d) (<= (length d-c-conns) (apply max SEG#))))
          
          (define colors (map third d-c-conns))
          (define others (remove* colors COLORS))
          (list c d (random-pick others) 3)))

      (cond
        [under-connected-cities under-connected-cities]
        [else (error 'choose-action "can't find a non-existent connection")]))))

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
  
  (define cheat-tri   (new cheat-strategy% [the-game-map vtriangle] [rails# 45]))

  (check-false (let* ([s (send cheat-tri choose-action pstate1)]
                      [t (game-map-all-connections vtriangle)])
                 (set-member? t s))))