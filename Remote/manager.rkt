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
;; the `receiver` is supposed to be a function that handles the client of a remote-call interaction 
;; -- its argument is called on the received JSON or EOF turned into JSxpr or EOF
;;    and its result is what the `receiver` turns back into a remote reply
;;    [I have developed a library that sets up both a sender and a receiver.]
;;    
;; `create-reply` is the best name I could come up with for the argument of the `receiver`

(provide
 (contract-out
  [make-remote-manager remote-manager/c]
  [pick-manager        (-> string? remote-manager/c)]))

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
(require SwDev/Testing/make-client)

(module+ test
  (require (submod ".."))
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/map-serialize examples))
  (require (submod Trains/Common/state-serialize examples))
  (require Trains/Common/connection)
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


(define-remote-manager make-remote-manager 
  [[start boolean] game-map]
  [[setup game-map natural color*] void]
  [[pick  destination-set] destination-set]
  [[play  pstate] action]
  [[more  color*] void]
  [[win   boolean] void]
  ;; when the last clause matches, the dispatcher shuts down 
  [[end   boolean] void])

(define-remote-manager make-remote-baddy-manager 
  [[start boolean] ill-formed-game-map]
  [[setup game-map natural color*] void]
  [[pick  destination-set] destination-set]
  [[play  pstate] action]
  [[more  color*] void]
  [[win   boolean] void]
  ;; when the last clause matches, the dispatcher shuts down 
  [[end   boolean] void])


(define manager-list
  [list [list "make-remote-baddy-manager" make-remote-baddy-manager]])

(define (pick-manager x)
  (define r (assoc x manager-list))
  (and r (second r)))

(require (only-in json jsexpr->string))

;; turn the map into a string that lacks the closing "}"
(define (ill-formed-game-map->jsexpr gm)
  (define jgm (game-map->jsexpr gm))
  (define sgm (jsexpr->string jgm))
  [broken (~a (make-string 4090 #\space) (substring sgm 0 (sub1 (string-length sgm))))])

;; turn the map into one that is missing the first city 
(define (invalid-game-map->jsexpr gm)
  (define jgm (game-map->jsexpr gm))
  (define sgm (jsexpr->string (hash-update jgm 'cities rest)))
  [broken sgm])

;; turn the action into a string that lacks the last char 
(define (ill-formed-action->jsexpr a)
  (define ja (action->jsexpr a))
  (define sa (jsexpr->string ja))
  [broken (substring sa 0 (sub1 (string-length sa)))])

;; turn the action into a plain word, w/o quotes when it comes in "over the wire"
(define (non-json-action->jsexpr a)
  [broken "MORE"])

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
  (define plr1 (new player% [the-map vtriangle] [strategy% hold-10-strategy%]))

  (define (dests n [convert (λ (a b) (list (~a a) (~a b)))])
    (define targets
      (build-list n (λ (i) (string->symbol (~a "Seat" (integer->char (+ (char->integer #\a) i)))))))
    (map convert (make-list n 'Boston) targets))

  (define (run j)
    (define (receiver dispatcher)
      (escape (dispatcher j)))
    (define escape #f)
    (define rm (make-remote-manager receiver))
    (let/cc k
      (set! escape k)
      (rm plr1)))

  (check-false [run eof])
  (check-equal? (run `["start" [#true]]) vtriangle-serialized)
  (check-equal? (run `["setup" [,vtriangle-serialized 4 ["red" "red"]]]) "void")
  (check-equal? (run  `["pick" [,(dests 5)]]) (drop (dests 5) 2))
  (check-equal? (run `["play" [,pstate1-serialized]]) MORE)
  (check-equal? (run `["more" [["blue" "blue" "red"]]]) "void" "tecn. ill")
  (check-equal? (run `["win" [#true]]) "void")
  (check-equal? (run `["end" [#false]]) "void")
  (check-equal? (run `["end" [#true]]) "void")

  (check-equal? ((make-remote-manager (λ (f) (f `["end" [#true]]))) plr1) #true)
  
  (define b*
    `[["start" [#true]]
      ["setup" [,vtriangle-serialized 3 ["red" "red"]]]
      ["pick" [,(dests 5)]]
      ["play" [,pstate1-serialized]]
      ["win" [#true]]
      ["end" [#true]]])
  (check-equal? ((make-remote-manager (λ (f) (begin0 (f (first b*)) (set! b* (rest b*))))) plr1) #t)
 
  (check-pred exn? (dev/null ((make-remote-manager (λ (f) (f `[0 [#true]]))) plr1))))

;; ---------------------------------------------------------------------------------------------------
;; test the broken-generating manager 
(module+ test
  (check-pred broken? (invalid-game-map->jsexpr vtriangle))
  (check-pred broken? (ill-formed-game-map->jsexpr vtriangle))

  (check-pred broken? (ill-formed-action->jsexpr [connection 'Boston 'Chicago 'blue 3]))
  (check-pred broken? (non-json-action->jsexpr [connection 'Boston 'Chicago 'blue 3])))