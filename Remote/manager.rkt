#lang racket

;; a remote manager that connects a single player to a client system, which connects to a server 

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

(require (only-in Trains/Common/player-interface manager-player/c))
(require (only-in json jsexpr?))

(define create-reply/c   (-> (or/c eof-object? jsexpr?) jsexpr?))
(define receiver/c       (-> create-reply/c any))
(define remote-manager/c (-> receiver/c (-> manager-player/c any)))

;; the `remote-manager` for a player is a function that
;; -- repeatedly receives JSexpr and turns them into arguments so that it can 
;; -- call the appropriate method in the given player and then
;; -- turn the result into a JSexpr that can be sent back 
;; 
;; the `receiver` is supposed to be a function that handles the side of a remote-call interaction 
;; -- its argument is called on the received JSON or EOF turned into JSxpr or EOF
;;    and its result is what the `receiver` turns back into a remote reply
;;    [I have developed a library that sets up both a sender and a receiver.]
;;    
;; `create-reply` is the best name I could come up with for the argument of the `receiver`

(provide (contract-out [make-remote-manager remote-manager/c]))

;                                                                                      
;       ;                                  ;                                           
;       ;                                  ;                          ;                
;       ;                                  ;                                           
;    ;;;;   ;;;   ;;;;    ;;;   ; ;;    ;;;;   ;;;   ; ;;    ;;;    ;;;    ;;;    ;;;  
;   ;; ;;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;;  ;     ;   ;;  ;  ;   ; 
;   ;   ;  ;   ;; ;   ;  ;   ;; ;   ;  ;   ;  ;   ;; ;   ;  ;         ;   ;   ;; ;     
;   ;   ;  ;;;;;; ;   ;  ;;;;;; ;   ;  ;   ;  ;;;;;; ;   ;  ;         ;   ;;;;;;  ;;;  
;   ;   ;  ;      ;   ;  ;      ;   ;  ;   ;  ;      ;   ;  ;         ;   ;          ; 
;   ;; ;;  ;      ;; ;;  ;      ;   ;  ;; ;;  ;      ;   ;  ;;        ;   ;      ;   ; 
;    ;;;;   ;;;;  ;;;;    ;;;;  ;   ;   ;;;;   ;;;;  ;   ;   ;;;;   ;;;;;  ;;;;   ;;;  
;                 ;                                                                    
;                 ;                                                                    
;                 ;                                                                    

(require Trains/Remote/define-dispatcher)
(require Trains/Common/json)
(require Trains/Common/action-serialize)
(require Trains/Common/basic-constants-serialize)
(require Trains/Common/map-serialize)
(require (except-in Trains/Common/player-interface manager-player/c))
(require Trains/Common/state-serialize)
(require Trains/Common/state)
(require Trains/Common/map)
(require SwDev/Testing/communication)

(module+ test
  (require (submod ".."))
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/map-serialize examples))
  (require (submod Trains/Common/state-serialize examples))
  (require Trains/Common/state)
  (require Trains/Player/player)
  (require Trains/Player/hold-10-strategy)
  (require SwDev/Lib/should-be-racket)
  (require rackunit))

;                                                                               
;                                                        ;                      
;                                                        ;            ;         
;                                                        ;                      
;   ;;;;    ;;;;   ;;;   ;   ;  ;   ;         ;;;;    ;;;; ;;;;;;   ;;;   ; ;;  
;   ;; ;;   ;;  ; ;; ;;   ; ;   ;   ;             ;  ;; ;; ;  ;  ;    ;   ;;  ; 
;   ;   ;   ;     ;   ;   ;;;    ; ;              ;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;   ;   ;     ;   ;    ;     ; ;           ;;;;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;   ;   ;     ;   ;   ;;;    ; ;          ;   ;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;; ;;   ;     ;; ;;   ; ;    ;;           ;   ;  ;; ;; ;  ;  ;    ;   ;   ; 
;   ;;;;    ;      ;;;   ;   ;    ;            ;;;;   ;;;; ;  ;  ;  ;;;;; ;   ; 
;   ;                             ;                                             
;   ;                            ;                                              
;   ;                           ;;


(define ((make-remote-manager receiver) player)
  (define done? (box (gensym)))
  (define dispatcher (make-dispatcher player done?))
  (parameterize ([io-time-out 1000])
    (let loop ()
      (with-handlers ([void (λ (xn)
                              (fprintf (current-error-port) "~a" (exn-message xn))
                              (set-box! done? xn))])
        (receiver dispatcher)
        (unless (boolean? (unbox done?))
          (loop)))))
  (unbox done?))

#;{XPlayer [Box (U Symbol Boolean)] -> ((U EOF JSexpr)  -> JSexpr)}
(define-dispatcher make-dispatcher
  [[start boolean] game-map]
  [[setup game-map natural color*] void]
  [[pick  destination-set] destination-set]
  [[play  pstate] action]
  [[more  color*] void]
  [[win   boolean] void]
  ;; when the last clause matches, the dispatcher signals the end of the cycle by setting done? to #t
  [[end   boolean] void])

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  (define player1 (new player% [the-map vtriangle] [strategy% hold-10-strategy%]))
  (define box1    (box (gensym)))

  (check-false [(make-dispatcher player1 box1) eof])

  (check-equal? [(make-dispatcher player1 box1) `["start" [#true]]] vtriangle-serialized)
  (check-true (symbol? (unbox box1)))

  (check-equal? [(make-dispatcher player1 box1) `["setup" [,vtriangle-serialized 4 ["red" "red"]]]]
                "void")
  (check-true (symbol? (unbox box1)))

  (define (dests n [convert (λ (a b) (list (~a a) (~a b)))])
    (define targets
      (build-list n (λ (i) (string->symbol (~a "Seat" (integer->char (+ (char->integer #\a) i)))))))
    (map convert (make-list n 'Boston) targets))
  (check-equal? [(make-dispatcher player1 box1) `["pick" [,(dests 5)]]] (drop (dests 5) 2))
  (check-true (symbol? (unbox box1)))

  (check-equal? [(make-dispatcher player1 box1) `["play" [,pstate1-serialized]]] MORE)
  (check-true (symbol? (unbox box1)))

  (check-equal? [(make-dispatcher player1 box1) `["more" [["blue" "blue" "red"]]]] "void" "tecn. ill")
  (check-true (symbol? (unbox box1)))

  (check-equal? [(make-dispatcher player1 box1) `["win" [#true]]] "void")
  (check-true (symbol? (unbox box1)))

  (check-equal? [(make-dispatcher player1 box1) `["end" [#false]]] "void")
  (check-true (boolean? (unbox box1)))

  (check-true ((make-remote-manager (λ (f) (f `["end" [#true]]))) player1))
  
  (define b*
    `[["start" [#true]]
      ["setup" [,vtriangle-serialized 3 ["red" "red"]]]
      ["pick" [,(dests 5)]]
      ["play" [,pstate1-serialized]]
      ["win" [#true]]
      ["end" [#true]]])
  (check-true ((make-remote-manager (λ (f) (begin0 (f (first b*)) (set! b* (rest b*))))) player1))
 
  (check-pred exn? (dev/null ((make-remote-manager (λ (f) (f `[0 [#true]]))) player1))))
