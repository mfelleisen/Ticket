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
   (->* [(or/c module-path? path-string?)] (#:gm game-map? #:name (or/c string? symbol?))
        (instanceof/c referee-player%/c))]
  [player% referee-player%/c]
  [player-bad-start% referee-player%/c]
  [player-bad-end% referee-player%/c]
  [player-bad-setup% referee-player%/c]
  [player-bad-pick% referee-player%/c]
  [player-bad-play% referee-player%/c]
  [player-bad-more% referee-player%/c]
  [player-bad-win% referee-player%/c]))

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
(require Trains/Player/buy-now-strategy)
(require (for-syntax syntax/parse))
(require (for-syntax racket))
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
                                         
(define player%
  (class object% [init-field strategy% [name (gensym 'player)] [the-map #false] [quiet #true]]
    (field [strategy #false])
    (define/public (start x) the-map)
    (define/public (end x) 'thanks)
    [define/public (setup gm r _) (set! strategy (new strategy% [the-game-map gm] [rails# r]))]
    [define/public (pick destinations) (send strategy pick-destinations destinations)]
    [define/public (play s) (send strategy choose-action s)]
    [define/public (more cards) (void)]
    [define/public (win did-i-win?)
      (unless quiet (displayln `[,name did ,(if did-i-win? 'WIN 'LOSE)]))]
    (super-new)))

;                                                          
;                                                          
;   ;                    ;       ;     ;                   
;   ;                    ;       ;                         
;   ;                    ;       ;                         
;   ; ;;;     ;;;    ;;; ;   ;;; ;   ;;;     ;;;;    ;;;;  
;   ;;  ;;   ;   ;  ;;  ;;  ;;  ;;     ;    ;    ;  ;    ; 
;   ;    ;       ;  ;    ;  ;    ;     ;    ;;;;;;  ;      
;   ;    ;   ;;;;;  ;    ;  ;    ;     ;    ;        ;;;;  
;   ;    ;  ;    ;  ;    ;  ;    ;     ;    ;            ; 
;   ;;  ;;  ;   ;;  ;;  ;;  ;;  ;;     ;    ;;   ;  ;    ; 
;   ; ;;;    ;;; ;   ;;; ;   ;;; ;   ;;;;;   ;;;;;   ;;;;  
;                                                          
;                                                          
;                                                          
;                                                          

(define-syntax (def/broken stx)
  (syntax-parse stx
    [(_ name:id (~optional n:number #:defaults ([n #'0])))
     #:do [(define class-id    (string->symbol (~a "player-bad-" (syntax-e #'name) "%")))
           (define class-name% (datum->syntax stx class-id stx stx))]
     #`(define #,class-name%
         (class player%
           (super-new)
           (define count n)
           (define/override (name . x)
             (cond
               [(> count 0) (set! count (- count 1)) (super name . x)]
               [else (let L () (sleep 1) (log-error "looping") (L))]))))]))

(def/broken start)
(def/broken end)
(def/broken setup)
(def/broken pick)
(def/broken play 3)
(def/broken more)
(def/broken win)


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

  (define p1-static (new player% [strategy% hold-10-strategy%]))

  (check-true (void? (send p1-static setup vtriangle 45 '[red red blue blue])))

  (check-equal? (set-count (send p1-static pick destinations)) 3)
  (check-equal? (send p1-static play pstate1) MORE)
  (check-true (void? (send p1-static more '[green blue])))
  (check-true (connection/c (send p1-static play pstate2)))
  (check-true (void? (dev/null (send p1-static win #false)))))

(module+ test ; simple tests with dynamically loaded hold-10-strategy to make sure the mechanics work 

  (define p1-dynamic (make-player-from-strategy-path 'Trains/Player/hold-10-strategy))

  (check-true (void? (send p1-dynamic setup vtriangle 45 '[red red blue blue])))

  (check-equal? (set-count (send p1-dynamic pick destinations)) 3)
  (check-equal? (send p1-dynamic play pstate1) MORE)
  (check-true (void? (send p1-dynamic more '[green blue])))
  (check-true (connection/c (send p1-dynamic play pstate2)))
  (check-true (void? (dev/null (send p1-dynamic win #false)))))
