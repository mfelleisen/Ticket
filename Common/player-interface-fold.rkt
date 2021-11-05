#lang racket

;; the player interface

;                                                          
;                                                          
;                                                          
;                                             ;            
;                                             ;            
;    ;;;;   ;;  ;;  ; ;;;    ;;;;    ;;;;   ;;;;;;   ;;;;  
;   ;    ;   ;  ;   ;;  ;;  ;;  ;;   ;;  ;    ;     ;    ; 
;   ;;;;;;    ;;    ;    ;  ;    ;   ;        ;     ;      
;   ;         ;;    ;    ;  ;    ;   ;        ;      ;;;;  
;   ;         ;;    ;    ;  ;    ;   ;        ;          ; 
;   ;;   ;   ;  ;   ;;  ;;  ;;  ;;   ;        ;     ;    ; 
;    ;;;;;  ;    ;  ; ;;;    ;;;;    ;         ;;;   ;;;;  
;                   ;                                      
;                   ;                                      
;                   ;                                      
;                                                          

(provide
 referee-player%/c
 manager-player%/c

 manager-player/c
 action?
 MORE)

;                                                                                                  
;                                                                                                  
;        ;                                       ;                             ;                   
;        ;                                       ;                                                 
;        ;                                       ;                                                 
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ; ;;;    ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;  
;   ;;  ;;  ;    ;  ;;  ;;  ;    ;  ;;   ;  ;;  ;;  ;    ;  ;;   ;   ;   ;     ;    ;    ;  ;    ; 
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;    ;  ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;      
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;        ;;;;  
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;            ; 
;   ;;  ;;  ;;   ;  ;;  ;;  ;;   ;  ;    ;  ;;  ;;  ;;   ;  ;    ;   ;   ;     ;    ;;   ;  ;    ; 
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;    ;   ;;; ;   ;;;;;  ;    ;    ;;;    ;;;;;   ;;;;;   ;;;;  
;                   ;                                                                              
;                   ;                                                                              
;                   ;                                                                              
;                                                                                                  

(require Trains/Common/basic-constants)
(require Trains/Common/connection)
(require Trains/Common/map)
(require Trains/Common/state)

(require trace-contract)

;                                                                          
;                                                                          
;      ;                                      ;                            
;                     ;                       ;                            
;                     ;                       ;                            
;    ;;;    ; ;;;   ;;;;;;   ;;;;    ;;;;   ;;;;;     ;;;     ;;;    ;;;;  
;      ;    ;;   ;    ;     ;    ;   ;;  ;    ;      ;   ;   ;   ;  ;    ; 
;      ;    ;    ;    ;     ;;;;;;   ;        ;          ;  ;       ;;;;;; 
;      ;    ;    ;    ;     ;        ;        ;      ;;;;;  ;       ;      
;      ;    ;    ;    ;     ;        ;        ;     ;    ;  ;       ;      
;      ;    ;    ;    ;     ;;   ;   ;        ;     ;   ;;   ;   ;  ;;   ; 
;    ;;;;;  ;    ;     ;;;   ;;;;;   ;        ;      ;;; ;    ;;;    ;;;;; 
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define MORE "more cards")
(define action? connection/c)

(define-syntax-rule (tag-trace t) (list/t 't t))
(define FAILED (gensym))

(require SwDev/Debugging/spy)

;; checks the protocol specified above 
(define proper-call-order?
  ;; poor mam's object
  (let (
        [PPM `[pick play more]])

    ;; debugging aid
    (define (show-error last-seen lst trace0)
      (displayln `[last-seen is ,last-seen lst is ,lst] (current-error-port))
      (displayln `[trace0 is ,(for/list ([x (in-stream trace0)]) x)] (current-error-port))
      FAILED)
    
    (λ (trace-0 [last-seen #false])

      ;; for proper debugging, I needed this:
      (define-syntax-rule (-> lst then)
        (if (member last-seen lst) then (show-error last-seen lst trace-0)))
      
      (match-define `[,one ,two] trace-0)
      
      (and
       (cond
         [(false? last-seen) (if (equal? 'setup one) one FAILED)]
         [(eq? last-seen #t) (if (equal? 'setup one) one FAILED)]
         [else 
           (case one
             [(setup) (-> '(win)   one)]
             [(pick)  (-> '[setup] one)]
             ;; not sure why I need equal? instead of eq? :::
             [(play)  (-> PPM      (if (equal? MORE two) 'more one))]
             [(more)  (-> '[more]  one)]
             [(win)   (-> PPM      (if (false? two) #true one))]
             [else    FAILED])])))))

(define referee-player%/c
  (trace/c ([setup any/c]
            [pick  set?]
            [play  (or/c action? MORE)]
            [more  (listof color?)]
            [win   boolean?])
    
    (class/c
     [setup
      ;; hand the player the map for the game, a number of rails, and some cards
      (->m game-map? natural? (listof color?) (tag-trace setup))]
     
     [pick
      ;; ask the player to pick some destinations and to return the remainder 
      (->m (set/c destination/c) (tag-trace pick))]
     
     [play
      ;; grant the player the right to take a turn 
      (->m pstate? (tag-trace play))]

     [more
      ;; if the preceding call to `play` returned `MORE`, call `more` to hand out more cards
      (->m (listof color?) (tag-trace more))]
     
     [win
      ;; inform the player whether it won (#t)/lost (#f) the game 
      (->m (tag-trace win) any)])
    
    ;; A proper game interaction sequence is a word in this regular expression:
    ;;    setup, pick, {play | more}*, win
    ;; meaning the referee calls the player with setup, followed by one call to pick,
    ;; followed by an arbitrary number of calls to `play` and possibly `more`, with
    ;; one final call to `win` at the very end of a game.
    #:fold 
    ((setup pick play more win) proper-call-order? #:fail-when (λ (x) (eq? FAILED x)))))

;; ---------------------------------------------------------------------------------------------------
#; {Stream : not enpty -> Boolean}

(define manager-player%/c
  (class/c
   [start (->m boolean? game-map?)]
   [end   (->m boolean? any)]))

(define manager-player/c (instanceof/c manager-player%/c))
