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

(require Trains/Player/istrategy)

(provide

 #; {type XPlayer}

 (contract-out
  ;; make a player from a path to a strategy module-file and an optional game-map and name
  [make-player-from-strategy-path
   (->* [(or/c module-path? path-string?)]
        (#:gm game-map? #:name (or/c string? symbol?))
        (instanceof/c referee-player%/c))]

  [make-player
   ;; make a player from a strategy class and an optional game-map and name
   (->* [#:strategy strategy/c%]
        (#:gm game-map? #:name (or/c string? symbol?))
        (instanceof/c referee-player%/c))]

  #;
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
(require Trains/Common/map)
(require Trains/Common/connection)
(module+ test
  (require (submod ".."))
  (require (submod Trains/Player/astrategy examples))
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples))
  (require Trains/Player/hold-10-strategy)
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

(define (make-player-from-strategy-path p #:name [name (gensym 'dynamic)] #:gm [gm #f])
  (define strat% (dynamic-require p 'strategy%))
  (new player% [strategy% strat%] [name name] [the-map gm]))

(define (make-player #:strategy strat% #:name [name (gensym 'dynamic)] #:gm [gm #f])
  (new player% [strategy% strat%] [name name] [the-map gm]))
                                         
(define player%
  (class object% [init-field strategy% [name (gensym 'player)] [the-map #false] [quiet #true]]
    (field [strategy #false])

    (define/public (start . x) the-map)
    (define/public (end . x) 'thanks)

    [define/public (setup gm rails cards)
      (set! strategy (new strategy% [the-game-map gm] [rails# rails]))
      DONE]

    [define/public (pick destinations)
      (send strategy pick-destinations destinations)]

    [define/public (play s)
      (send strategy choose-action s)]

    [define/public (more cards)
      cards]

    [define/public (win did-i-win?)
      (unless quiet 
        (displayln `[,name did ,(if did-i-win? 'WIN 'LOSE)]))
      OKAY]
    
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

(module+ test ;; simple tests with hold-10-strategy to make sure the mechanics work 

  (define p1-static (make-player #:strategy hold-10-strategy%))

  (check-equal? (send p1-static setup vtriangle 45 '[red red blue blue]) 'done)

  (check-equal? (set-count (send p1-static pick destinations)) 3)
  (check-equal? (send p1-static play pstate1) MORE)
  (check-equal? (send p1-static more '[green blue]) '[green blue])
  (check-true (connection/c (send p1-static play pstate2)))
  (check-equal? (dev/null (send p1-static win #false)) 'okay))

(module+ test ; simple tests with dynamically loaded hold-10-strategy to make sure the mechanics work 

  (define p1-dynamic (make-player-from-strategy-path 'Trains/Player/hold-10-strategy))

  (check-equal? (send p1-dynamic setup vtriangle 45 '[red red blue blue]) 'done)

  (check-equal? (set-count (send p1-dynamic pick destinations)) 3)
  (check-equal? (send p1-dynamic play pstate1) MORE)
  (check-equal? (send p1-dynamic more '[green blue])  '[green blue])
  (check-true (connection/c (send p1-dynamic play pstate2)))
  (check-equal? (dev/null (send p1-dynamic win #false)) 'okay))
