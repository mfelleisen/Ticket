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
  (trace/c ([r (or/c DONE set? action? MORE (listof color?)  OKAY)])
    ;; this makes the results less concrete, ARGH 
    (class/c
     ;; hand the player the map for the game, a number of rails, and some cards
     [setup (->m game-map? natural? (listof color?) r)]

     ;; ask the player to pick some destinations and to return the remainder 
     [pick  (->m (set/c destination/c) r)]

     ;; grant the player the right to take a turn 
     [play  (->m pstate? r)]

     ;; if the preceding call to `play` returned `MORE`, call `more` to hand out more cards
     [more  (->m (listof color?) r)]

     ;; inform the player whether it won (#t)/lost (#f) the game 
     [win   (->m boolean? r)])
    ((r) (proper-call-order? r))))

;; A proper game interaction sequence is a word in this regular expression:
;;    setup, pick, {play | more}*, win
;; meaning the referee calls the player with setup, followed by one call to pick,
;; followed by an arbitrary number of calls to `play` and possibly `more`, with
;; one final call to `win` at the very end of a game. 

#; {Stream -> Boolean}
(define (proper-call-order? trace0)
  (define starter (stream-first trace0))

  ; (displayln `[str is ,(for/list ([x (in-stream trace0)]) x)] (current-error-port))

  (and
   (equal? starter DONE)
   (let loop ([t (stream-rest trace0)] [last-seen starter])
     (cond
       [(stream-empty? t) #true]
       [else (define one (stream-first t))
             (define others (stream-rest t))

             ; (displayln `[one is ,one] (current-error-port))

             (cond
               [(equal? DONE one) #false]
               [(set? one)        (and (equal? last-seen DONE) (loop others 'pick))]
               [(action? one)     (and (member last-seen '[pick play more]) (loop others 'play))]
               [(equal? MORE one) (and (member last-seen '[pick play more]) (loop others 'more))]
               [(list? one)       (and (equal? last-seen 'more) (loop others 'play))]
               [(equal? OKAY one) (and (member last-seen '[pick play more]) (stream-empty? others))]
               [else #false])]))))

(define manager-player%/c
  (class/c
   [start (->m boolean? game-map?)]
   [end   (->m boolean? any)]))

(define manager-player/c (instanceof/c manager-player%/c))
