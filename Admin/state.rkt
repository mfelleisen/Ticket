#lang racket

;; representation of a referee's knowledge about the game

(provide
 #; {RefereeState -> RefereeState}
 ;; ASSUME there is a first player 
 rstate-rotate

 #; {RefereeState -> RefereeState}
 ;; ASSUME there is a first player 
 rstate-drop

 legal-action?
 rstate->pstate
 (struct-out rstate))

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

(require Trains/Common/state)
(require Trains/Common/map)

(module+ examples
  (provide rstate1 ii1 ii2))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Trains/Common/state examples))
  (require (submod Trains/Common/map examples))
  (require rackunit))

;                                                                          
;                                                                          
;        ;                                                                 
;        ;            ;                                                    
;        ;            ;                                                    
;    ;;; ;    ;;;   ;;;;;;    ;;;            ;;;;;   ;;;;   ; ;;;          
;    ;  ;;   ;   ;    ;      ;   ;           ;;      ;  ;;  ;;  ;          
;   ;    ;       ;    ;          ;           ;      ;    ;  ;    ;         
;   ;    ;   ;;;;;    ;      ;;;;;           ;      ;;;;;;  ;    ;         
;   ;    ;  ;    ;    ;     ;    ;           ;      ;       ;    ;         
;   ;    ;  ;    ;    ;     ;    ;           ;      ;       ;    ;    ;;   
;    ;  ;;  ;   ;;    ;     ;   ;;           ;       ;      ;;  ;     ;;   
;    ;;; ;   ;;; ;     ;;;   ;;; ;           ;       ;;;;;  ; ;;;     ;;   
;                                                           ;              
;                                                           ;              
;                                                           ;              
;                                                                          

(struct rstate [players cards drop-outs] #:transparent)
#; {type RefereeState = (rstate [Listof [MePlayer XPlayer]] [Listof Cards] [Listof XPlayer])}
#; {type XPlayer      = .. see player interface ..}

;; the MeState must be parameterized over a paylaod 

(module+ examples
  (define cards1 (hash 'green 5))
  (define dest1  (set 'Boston 'Seattle))
  (define ii1 (ii dest1 '(oston Orlando) 40 cards1 (set) #f))
  (define ii2 (ii dest1 '(Orlando Seattle) 5 cards1 (set '[(Boston Seattle) red 3]) #f))

  (define rstate1 (rstate (list ii1 ii2) '[] '[]))

  (provide rstate1-r rstate1-d)
  (define rstate1-r (rstate (list ii2 ii1) '[] '[]))
  (define rstate1-d (rstate (list ii2) '[] `[,ii1])))

;                                                                          
;                                                                          
;      ;;;                                     ;                           
;     ;                               ;        ;                           
;     ;                               ;                                    
;   ;;;;;;  ;    ;  ; ;;;     ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;    ;;;;  
;     ;     ;    ;  ;;   ;   ;   ;    ;        ;    ;;  ;;  ;;   ;  ;    ; 
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;      
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;;;    
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;     ;;; 
;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;       ; 
;     ;     ;   ;;  ;    ;   ;   ;    ;        ;    ;;  ;;  ;    ;  ;    ; 
;     ;      ;;; ;  ;    ;    ;;;      ;;;  ;;;;;;;  ;;;;   ;    ;   ;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define (rstate-drop the-state)
  (match-define [rstate (cons first others) cards drop-outs] the-state)
  (rstate others cards (cons first drop-outs)))

#; {RefereeState -> RefereeState}
(define (rstate-rotate the-state)
  (match-define [rstate (cons first others) cards drop-outs] the-state)
  (rstate (append others (list first)) cards drop-outs))
  
#; {RefereeState -> PlayerState}
(define (rstate->pstate rs)
  (define players (map nuke-external (rstate-players rs)))
  (pstate (first players) (map ii-connections (rest players))))

#; {MePlayer -> MePlayer}
;; EFFECT drop external player 
(define (nuke-external iplayer)
  (set-ii-payload! iplayer #false)
  iplayer)

#; {Map RefereeState Connection -> Boolean}
;; can this player acquire the specified connection 
(define (legal-action? m rs c)
  (define players (rstate-players rs))
  (define total   (game-map-all-connections m))
  (define active  (first players))
  (define other   (apply set-union (map ii-connections players)))
  (define avail   (set-subtract total other))
  (cond
    [(not (set-member? avail c)) #false]
    [else (>= (hash-ref (ii-cards active) (connection-color c) 0) (connection-seg# c))]))

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

  (check-equal? (rstate-drop rstate1) rstate1-d)
  (check-equal? (rstate-rotate rstate1) rstate1-r)

  (check-equal? (rstate->pstate rstate1) (pstate ii1 (list (ii-connections ii2))))

  (check-false (legal-action? vtriangle rstate1 (list (set 'Boston 'Seattle) 'red  3)))
  (check-true (legal-action? vtriangle rstate1 (list 'Boston 'Orlando 'green  5))))
