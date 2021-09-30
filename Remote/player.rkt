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

(require (only-in Trains/Common/player-interface manager-player/c))

(provide
 (contract-out
  (make-remote-player
   (-> string? input-port? output-port? manager-player/c))))

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

(require Trains/Remote/define-remote)
(require Trains/Common/basic-constants-serialize)
(require Trains/Common/action-serialize)
(require Trains/Common/state-serialize)
(require Trains/Common/map-serialize)
(require (except-in Trains/Common/player-interface manager-player/c))
(require (except-in Trains/Common/json string->jsexpr))

(module+ test
  (require (submod ".."))
  (require Trains/Remote/remote-testing)
  (require (submod Trains/Common/state examples))
  (require (submod Trains/Common/map examples))
  (require (submod Trains/Common/map-serialize examples)))

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


(define (make-remote-player name in out)
  (new remote-player% [name name] [in in] [out out]))

(define remote-player%
  (class object% [init-field in out (strategy 'just-to-satisfy-the-contract) [name (gensym 'rem)]]
    (super-new)

    (define-define/remote define/remote in out)
    
    ;; -----------------------------------------------------------------------------------------------
    (define/remote (start boolean)                     game-map)
    (define/remote (end   boolean)                     void)
    
    (define/remote (setup game-map natural color*)     void)
    (define/remote (pick  [destination])               destination-set)
    (define/remote (play  pstate)                      action)
    (define/remote (more  color*)                      void)
    (define/remote (win   boolean)                     void)))

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
