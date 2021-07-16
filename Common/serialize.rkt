#lang racket

;; serialization of board maps to JSON 

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

#; {JGraph -> Boolean}
;; GURANATEE All connections between two cities A and B are specified in only
;; one hash entry (either from A to B or B to A). Use string<? to make this work.
(define (guarantee serialized-graph)
  (define serialized-graph-proper (hash-ref serialized-graph CONNECTIONS))
  (define dom (map car (hash->list serialized-graph-proper)))
  (define rng (map (λ (x) (map car (hash->list (hash-ref serialized-graph-proper x)))) dom))
  (for/and ([d dom] [r rng])
    (andmap (λ (r) (symbol<? d r)) r)))

(require (only-in json jsexpr?))

(provide
 CITIES
 WIDTH 
 HEIGHT
 CONNECTIONS
 
 (contract-out 
  [vgraph->jsexpr (-> any/c (and/c jsexpr? guarantee))]))

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

(require Trains/Common/board)

;                                                                                          
;                                                                                          
;                                                                                          
;     ;;;    ;;;;;                                                                         
;       ;   ;;                                                                             
;       ;   ;        ;;;;   ;;  ;;  ; ;;;    ;;;;            ;;;;    ;;;;   ; ;;;          
;       ;   ;;      ;    ;   ;  ;   ;;  ;;   ;;  ;           ;;  ;  ;    ;  ;;  ;;         
;       ;    ;;;;;  ;;;;;;    ;;    ;    ;   ;               ;      ;;;;;;  ;    ;         
;       ;        ;  ;         ;;    ;    ;   ;               ;      ;       ;    ;         
;       ;           ;         ;;    ;    ;   ;               ;      ;       ;    ;    ;;   
;   ;   ;   ;    ;  ;;   ;   ;  ;   ;;  ;;   ;               ;      ;;   ;  ;;  ;;    ;;   
;    ;;;     ;;;;;   ;;;;;  ;    ;  ; ;;;    ;               ;       ;;;;;  ; ;;;     ;;   
;                                   ;                                       ;              
;                                   ;                                       ;              
;                                   ;                                       ;              
;                                                                                          

(define CITIES 'cities)
(define WIDTH 'width)
(define HEIGHT 'height)
(define CONNECTIONS 'connections)

#; {type Graph  = [Hash WIDTH HEIGHT CITIES CONNECTIONS]}
#; {type JGraph = [Hashof Symbol JSlice]}
#; {type JSlice = [Hashof Symbol JColor]}
#; {type JColor = [Hashof Color Natural]}
;; The hash table maps city names to city names, which are mapped to colors
;; and those map to the number of segments. All connections are bi-directional.

(define triangle-serialized
  (hash 'Boston  (hash 'Orlando (hash 'green 5
                                      'white 3)
                       'Seattle  (hash 'green 4
                                       'red 3))
        'Orlando (hash 'Seattle  (hash 'blue 5))
        'Seattle (hash)))

(define vtriangle-serialized
  (hash CITIES      '(("Boston" (0 0)) ("Seattle" (0 0)) ("Orlando" (0 0)))
        CONNECTIONS triangle-serialized
        HEIGHT      0
        WIDTH       0))

;                                                                          
;                                                                          
;                              ;             ;;;       ;                   
;                                              ;                           
;                                              ;                           
;    ;;;;    ;;;;    ;;;;    ;;;      ;;;      ;     ;;;    ;;;;;;   ;;;;  
;   ;    ;  ;    ;   ;;  ;     ;     ;   ;     ;       ;        ;;  ;    ; 
;   ;       ;;;;;;   ;         ;         ;     ;       ;       ;;   ;;;;;; 
;    ;;;;   ;        ;         ;     ;;;;;     ;       ;      ;;    ;      
;        ;  ;        ;         ;    ;    ;     ;       ;     ;;     ;      
;   ;    ;  ;;   ;   ;         ;    ;   ;;     ;       ;    ;;      ;;   ; 
;    ;;;;    ;;;;;   ;       ;;;;;   ;;; ;      ;;;  ;;;;;  ;;;;;;   ;;;;; 
;                                                                          
;                                                                          
;                                                                          
;                                                                          

#; {Nod* Connection* Image -> MAP}
(define (vgraph->jsexpr g)
  (hash WIDTH  (graph-width g)
        HEIGHT (graph-height g)
        CITIES (graph-locations g)
        CONNECTIONS (graph->jsexpr g)))

#; {Graph -> JGraph}
(define (graph->jsexpr graph)
  (define cities (graph-cities graph))
  (for/hash ([c cities])
    (values c (to* graph c))))

#; {Graph City -> JSlice}
(define (to* graph city)
  (for*/hash ([connection* (group-by to-city (graph-connections graph city))]
              [next-city   (in-value (to-city (first connection*)))]
              #:when (symbol<? city next-city))
    (define color+seg# (to-color+seg# connection*))
    (values next-city (connected-via color+seg#))))

#; {[Listof Connection] -> JColors}
(define (connected-via connection*)
  (for/hash ([c (group-by first connection*)])
    (values (first (first c)) (second (first c)))))

;                                          
;                                          
;                                          
;     ;                       ;            
;     ;                       ;            
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;  
;     ;     ;    ;  ;    ;    ;     ;    ; 
;     ;     ;;;;;;  ;         ;     ;      
;     ;     ;        ;;;;     ;      ;;;;  
;     ;     ;            ;    ;          ; 
;     ;     ;;   ;  ;    ;    ;     ;    ; 
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;  
;                                          
;                                          
;                                          
;                                          

(module+ test
  (require (submod ".."))
  (require (submod Trains/Common/board examples))
  (require rackunit)
  
  (check-equal? (vgraph->jsexpr vtriangle) vtriangle-serialized  "triangle"))