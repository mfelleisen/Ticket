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
 MORE
 DONE
 OKAY)

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

(define strategy%/c
  (class/c))

(define DONE 'done)
(define OKAY 'okay)

(define referee-player%/c
  (trace/c ([r DONE]
            [s set?]
            [t (or/c action? MORE)]
            [u (listof color?)]
            [w OKAY])
    ;; this makes the results less concrete, ARGH 
    (class/c
     ;; hand the player the map for the game, a number of rails, and some cards
     [setup (->m game-map? natural? (listof color?) (list/t 'setup r))]

     ;; ask the player to pick some destinations and to return the remainder 
     [pick  (->m (set/c destination/c) (list/t 'pick s))]

     ;; grant the player the right to take a turn 
     [play  (->m pstate? (list/t 'play t))]

     ;; if the preceding call to `play` returned `MORE`, call `more` to hand out more cards
     [more  (->m (listof color?) (list/t 'more u))]

     ;; inform the player whether it won (#t)/lost (#f) the game 
     [win   (->m boolean? (list/t 'win w))])
    ((r s t u w) (proper-call-order? (trace-merge r s t u w)))))

;; A proper game interaction sequence is a word in this regular expression:
;;    setup, pick, {play | more}*, win
;; meaning the referee calls the player with setup, followed by one call to pick,
;; followed by an arbitrary number of calls to `play` and possibly `more`, with
;; one final call to `win` at the very end of a game.

(define LAST `[pick play more])

;; debugging aid
(define (show-error last-seen lst trace0 value0 t v)
  (displayln `[last-seen is ,last-seen lst is ,lst] (current-error-port))
  (displayln `[trace0 is ,(for/list ([x (in-stream trace0)]) x)] (current-error-port))
  (displayln `[value0 is ,(for/list ([x (in-stream trace0)]) x)] (current-error-port))
  (displayln `[failure at ,(for/list ([x (in-stream t)]) x)] (current-error-port))
  (displayln `[failure at ,(for/list ([x (in-stream v)]) x)] (current-error-port))
  #false)

#; {Stream -> Boolean}
(define (proper-call-order? trace-0)

  (define trace0  (stream-map first trace-0))
  (define values0 (stream-map second trace-0))
  (define starter (stream-first trace0))
  (and
   (equal? starter 'setup)
   (let L ([t (stream-rest trace0)] [v (stream-rest values0)] [last-seen starter])

     ;; for proper debugging, I needed this:
     (define-syntax-rule (-> lst then)
       (if (member last-seen lst) then (show-error last-seen lst trace0 values0 t v)))

     (cond
       [(stream-empty? t) #true]
       [else (define one (stream-first t))
             (define others (stream-rest t))
             (define two (stream-first v))
             (define values (stream-rest v))
             (case one
               [(setup) #false]
               [(pick)  (-> '[setup] (L others values 'pick))]
               [(play)  (-> LAST     (L others values (if (eq? MORE two) 'more one)))]
               [(more)  (-> `[more]  (L others values 'more))]
               [(win)   (-> LAST     (stream-empty? others))]
               [else    #false])]))))

(define manager-player%/c
  (class/c
   [start (->m boolean? game-map?)]
   [end   (->m boolean? any)]))

(define manager-player/c (instanceof/c manager-player%/c))
