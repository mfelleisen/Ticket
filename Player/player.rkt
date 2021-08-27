#lang racket

;; the mechanical part of the player (simplistic)

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

 #; {type XPlayer}

 (contract-out
  [make-player-from-strategy-path
   (->* [(or/c module-path? path-string?)] (#:name (or/c string? symbol?))
        (instanceof/c referee-player%/c))]
  [player% referee-player%/c]))

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

(require Trains/Common/player-interface)
(module+ test
  (require (submod ".."))
  (require (submod Trains/Player/astrategy examples))
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples))
  (require Trains/Player/simple-strategy)
  (require SwDev/Lib/should-be-racket)
  (require rackunit))

;                                                  
;                                                  
;            ;;;                                   
;              ;                                   
;              ;                                   
;   ; ;;;      ;      ;;;   ;    ;   ;;;;    ;;;;; 
;   ;;  ;      ;     ;   ;   ;   ;   ;  ;;   ;;    
;   ;    ;     ;         ;   ;  ;   ;    ;   ;     
;   ;    ;     ;     ;;;;;   ;  ;   ;;;;;;   ;     
;   ;    ;     ;    ;    ;    ; ;   ;        ;     
;   ;    ;     ;    ;    ;    ;;    ;        ;     
;   ;;  ;      ;    ;   ;;    ;;     ;       ;     
;   ; ;;;       ;;;  ;;; ;     ;     ;;;;;   ;     
;   ;                          ;                   
;   ;                         ;                    
;   ;                        ;;                    
;                                                  

(define (make-player-from-strategy-path p #:name [name (gensym 'dynamic)])
  (define strat% (dynamic-require p 'strategy%))
  (new player% [strategy% strat%] [name name]))
                                         
(define player%
  (class object% [init-field strategy% [name (gensym 'player)] [quiet #true]]
    (field [strategy #false])

    [define/public (setup gm rails cards)
      (set! strategy (new strategy% [the-game-map gm] [rails# rails]))]

    [define/public (pick destinations)
      (send strategy pick-destinations destinations)]

    [define/public (play s)
      (send strategy choose-action s)]

    [define/public (more cards)
      (void)]

    [define/public (win did-i-win?)
      (unless quiet 
        (displayln `[,name did ,(if did-i-win? 'WIN 'LOSE)]))]
    
    (super-new)))

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

(module+ test ;; simple tests with simple-strategy to make sure the mechanics work 

  (define p1-static (new player% [strategy% simple-strategy%]))

  (check-true (void? (send p1-static setup vtriangle 45 '[red red blue blue])))

  (check-equal? (set-count (send p1-static pick destinations)) 3)
  (check-equal? (send p1-static play pstate1) MORE)
  (check-true (void? (send p1-static more '[green blue])))
  (check-true (list? (send p1-static play pstate2)))
  (check-true (void? (dev/null (send p1-static win #false)))))

(module+ test ;; simple tests with dynamically loaded simple-strategy to make sure the mechanics work 

  (define p1-dynamic (make-player-from-strategy-path 'Trains/Player/simple-strategy))

  (check-true (void? (send p1-dynamic setup vtriangle 45 '[red red blue blue])))

  (check-equal? (set-count (send p1-dynamic pick destinations)) 3)
  (check-equal? (send p1-dynamic play pstate1) MORE)
  (check-true (void? (send p1-dynamic more '[green blue])))
  (check-true (list? (send p1-dynamic play pstate2)))
  (check-true (void? (dev/null (send p1-dynamic win #false)))))