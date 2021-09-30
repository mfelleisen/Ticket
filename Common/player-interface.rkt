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

(define referee-player%/c
  (class/c
   [init-field [strategy% strategy%/c] [name city?]]
   [setup (->m game-map? natural? (listof color?) any/c)]
   [pick  (->m (set/c destination/c) (set/c destination/c))]
   [play  (->m pstate? (or/c MORE action?))]
   [more  (->m (listof color?) any/c)]
   [win   (->m boolean? any/c)]))

(define manager-player%/c
  (class/c
   [start (->m boolean? game-map?)]
   [end   (->m boolean? any)]))

(define manager-player/c (instanceof/c manager-player%/c))
