#lang racket

;; this remote player implements the same interface as the player but conveys its arguments
;; to the given TCP out stream and receives the results on the TCP in stream

;                                                                  
;                                                                  
;                                                            ;;;   
;                     ;                                        ;   
;                     ;                                        ;   
;    ;;;;   ;;  ;;  ;;;;;;   ;;;;    ;;;;   ; ;;;     ;;;      ;   
;   ;    ;   ;  ;     ;     ;    ;   ;;  ;  ;;   ;   ;   ;     ;   
;   ;;;;;;    ;;      ;     ;;;;;;   ;      ;    ;       ;     ;   
;   ;         ;;      ;     ;        ;      ;    ;   ;;;;;     ;   
;   ;         ;;      ;     ;        ;      ;    ;  ;    ;     ;   
;   ;;   ;   ;  ;     ;     ;;   ;   ;      ;    ;  ;   ;;     ;   
;    ;;;;;  ;    ;     ;;;   ;;;;;   ;      ;    ;   ;;; ;      ;;;
;                                                                  
;                                                                  
;                                                                  
;                                                                  

(require Trains/Common/player-interface)

(provide
 ;; a contract that describes the player class's interface to the administrator 
 (contract-out
  (make-remote-player
   (-> input-port? output-port? manager-player/c))))

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

(require Trains/Common/basic-constants)
(require Trains/Common/state-serialize)
(require Trains/Common/map-serialize)
(require Trains/Common/map)
(require Trains/Common/player-interface)
(require json)

(require "define-remote.rkt")

(require SwDev/Testing/communication)

(module+ test
  (require (submod ".."))
  (require (submod Trains/Common/state examples))
  (require Trains/Common/state)
  (require SwDev/Debugging/diff)
  (require rackunit)
  (require (for-syntax syntax/parse)))

;                                            
;                                            
;          ;;;                               
;            ;                               
;   ;;;;     ;    ;;;;   ;   ;   ;;;    ;;;; 
;   ;; ;;    ;        ;  ;   ;  ;;  ;   ;;  ;
;   ;   ;    ;        ;   ; ;   ;   ;;  ;    
;   ;   ;    ;     ;;;;   ; ;   ;;;;;;  ;    
;   ;   ;    ;    ;   ;   ; ;   ;       ;    
;   ;; ;;    ;    ;   ;   ;;    ;       ;    
;   ;;;;      ;;   ;;;;    ;     ;;;;   ;    
;   ;                      ;                 
;   ;                     ;                  
;   ;                    ;;                  


(define (make-remote-player in out)
  (new remote-player% [in in] [out out]))

(define remote-player%
  (class object% [init-field in out (strategy 'just-to-satisfy-the-contract) [name (gensym 'rem)]]
    (super-new)

    (define-define/remote define/remote in out)
    
    ;; -----------------------------------------------------------------------------------------------
    (define/remote (start boolean)                     game-map)
    (define/remote (end   boolean)                     void)
    
    (define/remote (setup game-map natural color-list) void)
    (define/remote (pick  [destination])               destination-set)
    (define/remote (play  pstate)                      action)
    (define/remote (more  color-list)                  void)
    (define/remote (win   boolean)                     void)))

(define (natural->jsexpr n) n)
(define (boolean->jsexpr b) b)
(define (color-list->jsexpr loc) (map (λ (c) (~a c)) loc))

(define (parse-destination-set j)
  (match j
    [`[,(and dest `(,[? city? from] ,(? city? to))) ...]
     (apply set (map parse-destination dest))]))

(define (parse-void j) (match j ["void" (void)]))

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
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/map-serialize examples))

  (require "remote-testing.rkt")

  (define pstate1-serialized (pstate->jsexpr pstate1))

  (test-remote make-remote-player
               (start #t)
               #:remote vrectangle-serialized
               #:exp vrectangle
               #:msg '["start" [#t]])
  (test-remote make-remote-player
               (setup vtriangle 3 '[red blue])
               #:msg `["setup" [,vtriangle-serialized 3 ["red" "blue"]]])
  (test-remote make-remote-player
               (pick (apply set (make-list 5 '[Boston Seattle])))
               #:remote (make-list 3 '["Boston" "Seattle"])
               #:exp (apply set (make-list 3 '[Boston Seattle]))
               #:msg '("pick" [(["Boston" "Seattle"])]))
  (test-remote make-remote-player
               (play pstate1)
               #:remote MORE
               #:exp MORE
               #:msg `["play" [,pstate1-serialized]])
  (test-remote make-remote-player
               (play pstate1)
               #:remote '["Boston" "Seattle" "red" 3]
               #:exp '[Boston Seattle red 3]
               #:msg `["play" [,pstate1-serialized]])
  (test-remote make-remote-player
               (more '[blue red]) #:remote "void" #:exp (void) #:msg '["more" [["blue" "red"]]])
  (test-remote make-remote-player
               (win #f) #:msg '["win" [#f]])
  (test-remote make-remote-player
               (end #f) #:msg '["end" [#f]]))
