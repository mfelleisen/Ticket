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

(define receiver/c (-> (-> (or/c eof-object? jsexpr?) jsexpr?) any))

(provide
 (contract-out
  [make-remote-manager (-> receiver/c (-> manager-player/c any))]))

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
  (define disp  (dispatcher player done?))
  (parameterize ([io-time-out 1000])
    (let loop ()
      (with-handlers ([void (λ (xn)
                              (fprintf (current-error-port) "~a" (exn-message xn))
                              (set-box! done? xn))])
        (receiver disp)
        (unless (boolean? (unbox done?))
          (loop)))))
  (unbox done?))

#;{XPlayer [Box (U Symbol Boolean)] -> JSexpr -> JSexpr}
(define-dispatcher dispatcher
  [[start boolean] game-map]
  [[setup game-map natural color*] void]
  [[pick  destination-set] destination-set]
  [[play  pstate] action]
  [[more  color*] void]
  [[win   boolean] void]
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

  (check-false [(dispatcher player1 box1) eof])

  (check-equal? [(dispatcher player1 box1) `["start" [#true]]] vtriangle-serialized)
  (check-true (symbol? (unbox box1)))

  (check-equal? [(dispatcher player1 box1) `["setup" [,vtriangle-serialized 4 ["red" "red"]]]] "void")
  (check-true (symbol? (unbox box1)))

  (define (dests n [convert (λ (a b) (list (~a a) (~a b)))])
    (define targets
      (build-list n (λ (i) (string->symbol (~a "Seat" (integer->char (+ (char->integer #\a) i)))))))
    (map convert (make-list n 'Boston) targets))
  (check-equal? [(dispatcher player1 box1) `["pick" [,(dests 5)]]] (drop (dests 5) 2))
  (check-true (symbol? (unbox box1)))

  (check-equal? [(dispatcher player1 box1) `["play" [,pstate1-serialized]]] MORE)
  (check-true (symbol? (unbox box1)))

  (check-equal? [(dispatcher player1 box1) `["more" [["blue" "blue" "red"]]]] "void" "tecn. Illegale")
  (check-true (symbol? (unbox box1)))

  (check-equal? [(dispatcher player1 box1) `["win" [#true]]] "void")
  (check-true (symbol? (unbox box1)))

  (check-equal? [(dispatcher player1 box1) `["end" [#false]]] "void")
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
