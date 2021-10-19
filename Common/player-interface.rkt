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

(define referee-player%/c
  (trace/c ([setup any/c]
            [pick  set?]
            [play  (or/c action? MORE)]
            [more  (listof color?)]
            [win   any/c])
    (class/c
     ;; hand the player the map for the game, a number of rails, and some cards
     [setup (->m game-map? natural? (listof color?) (tag-trace setup))]

     ;; ask the player to pick some destinations and to return the remainder 
     [pick  (->m (set/c destination/c) (tag-trace pick))]

     ;; grant the player the right to take a turn 
     [play  (->m pstate? (tag-trace play))]

     ;; if the preceding call to `play` returned `MORE`, call `more` to hand out more cards
     [more  (->m (listof color?) (tag-trace more))]

     ;; inform the player whether it won (#t)/lost (#f) the game 
     [win   (->m boolean? (tag-trace win))])
    ((setup pick play more win) (proper-call-order? (trace-merge setup pick play more win)))))

;; A proper game interaction sequence is a word in this regular expression:
;;    setup, pick, {play | more}*, win
;; meaning the referee calls the player with setup, followed by one call to pick,
;; followed by an arbitrary number of calls to `play` and possibly `more`, with
;; one final call to `win` at the very end of a game.

(define PPM `[pick play more])

;; checks the protocol specified above 
(define (proper-call-order? trace-0)
  (define starter (first (stream-first trace-0)))
  (and
   (equal? 'setup starter)
   (let L ([t (stream-rest trace-0)] [last-seen starter])

     ;; for proper debugging, I needed this:
     (define-syntax-rule (-> lst then)
       (if (member last-seen lst) then (show-error last-seen lst trace-0 t)))

     (cond
       [(stream-empty? t) #true]
       [else (match-define `[,one ,two] (stream-first t))
             (define others (stream-rest t))
             (case one
               [(setup) #false]
               [(pick)  (-> '[setup] (L others 'pick))]
               [(play)  (-> PPM      (L others (if (eq? MORE two) 'more one)))]
               [(more)  (-> `[more]  (L others 'more))]
               [(win)   (-> PPM      (stream-empty? others))]
               [else    #false])]))))

;; debugging aid
(define (show-error last-seen lst trace0 t)
  (displayln `[last-seen is ,last-seen lst is ,lst] (current-error-port))
  (displayln `[trace0 is ,(for/list ([x (in-stream trace0)]) x)] (current-error-port))
  (displayln `[failure at ,(for/list ([x (in-stream t)]) x)] (current-error-port))
  #false)

;; ---------------------------------------------------------------------------------------------------
#; {Stream : not enpty -> Boolean}

(define manager-player%/c
  (class/c
   [start (->m boolean? game-map?)]
   [end   (->m boolean? any)]))

(define manager-player/c (instanceof/c manager-player%/c))
