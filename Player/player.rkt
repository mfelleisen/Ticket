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
  (require (submod Trains/Player/simple-strategy examples))
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/state examples))
  (require Trains/Player/simple-strategy)
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

(define player%
  (class object% [init-field strategy%]
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
      (displayln `[me ,did-i-win?])]
    
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
  (define-syntax-rule (dev-null e)
    (parameterize ([current-output-port (open-output-string)]) e))

  (define p1 (new player% [strategy% simple-strategy%]))

  (check-true (void? (send p1 setup vtriangle 45 '[red red blue blue])))

  (check-equal? (set-count (send p1 pick destinations)) 3)
  (check-equal? (send p1 play pstate1) MORE)
  (check-true (void? (send p1 more 'green 'blue)))
  (check-true (list? (send p1 play pstate2)))
  (check-true (void? (dev-null (send p1 win #false)))))
