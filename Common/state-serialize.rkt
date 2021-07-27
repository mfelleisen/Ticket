#lang racket

(provide

 DESTINATION
 THIS
 CONNECTIONS
 RAILS
 CARDS

 pstate->jsexpr
 acquired->jsexpr
 parse-state)
 

;                                                                                                  
;                                                                                                  
;        ;                                       ;                             ;                   
;        ;                                       ;                             ;                   
;        ;                                       ;                                                 
;    ;;; ;   ;;;;   ; ;;;    ;;;;   ; ;;;    ;;; ;   ;;;;   ; ;;;     ;;;    ;;;     ;;;;    ;;;;  
;    ;  ;;   ;  ;;  ;;  ;    ;  ;;  ;;   ;   ;  ;;   ;  ;;  ;;   ;   ;   ;     ;     ;  ;;  ;    ; 
;   ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;    ;  ;          ;    ;    ;  ;      
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;  ;    ;  ;    ;  ;;;;;;  ;    ;  ;          ;    ;;;;;;  ;;;    
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;          ;;; 
;   ;    ;  ;       ;    ;  ;       ;    ;  ;    ;  ;       ;    ;  ;          ;    ;            ; 
;    ;  ;;   ;      ;;  ;    ;      ;    ;   ;  ;;   ;      ;    ;   ;   ;     ;     ;      ;    ; 
;    ;;; ;   ;;;;;  ; ;;;    ;;;;;  ;    ;   ;;; ;   ;;;;;  ;    ;    ;;;   ;;;;;;;  ;;;;;   ;;;;  
;                   ;                                                                              
;                   ;                                                                              
;                   ;                                                                              
;                                                                                                  

(require Trains/Common/map)
(require Trains/Common/state)
(require Trains/Common/basic-constants)
(require SwDev/Testing/communication)

(module+ test
  (require (submod Trains/Common/state examples))
  (require (submod Trains/Common/map examples))
  (require rackunit)
  (require json))

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

(define (DESTINATION i) (string->symbol (~a "destination" i)))
(define THIS        'this)
(define CONNECTIONS 'acquired)
(define RAILS       'rails)
(define CARDS       'cards)

#; {type State  = [Hash [THIS This] [CONNECTIONS Conns]]}
#; {type This   = [Hash [(DESTINATION 1) Destination]
                        [(DESTINATION 2) Destination]
                        [RAILS N]
                        [CARDS Cards]
                        [CONNECTIONS Conns]]}
#; {type Conns  = [Listof Connection]}
#; {type Destination = [List String String]}
#; {type Cards = [Hash Color N]}
;; The two Strings denote cities, in string<? order. The path is bi-directional. 
#; {type Connection = [List String String Color Seg#]}
;; The two Strings denote cities, in string<? order. The connection is bi-directional. 

;                                                                          
;                                                                          
;                              ;             ;;;       ;                   
;                              ;               ;       ;                   
;                                              ;                           
;    ;;;;    ;;;;    ;;;;;   ;;;      ;;;      ;     ;;;    ;;;;;;   ;;;;  
;   ;    ;   ;  ;;   ;;        ;     ;   ;     ;       ;         ;   ;  ;; 
;   ;       ;    ;   ;         ;         ;     ;       ;        ;   ;    ; 
;   ;;;     ;;;;;;   ;         ;     ;;;;;     ;       ;       ;    ;;;;;; 
;      ;;;  ;        ;         ;    ;    ;     ;       ;      ;     ;      
;        ;  ;        ;         ;    ;    ;     ;       ;     ;      ;      
;   ;    ;   ;       ;         ;    ;   ;;     ;       ;    ;        ;     
;    ;;;;    ;;;;;   ;      ;;;;;;;  ;;; ;      ;;; ;;;;;;; ;;;;;;   ;;;;; 
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define (pstate->jsexpr ps)
  (hash THIS        (ii->jsexpr (pstate-I ps))
        CONNECTIONS (for/list ([x (pstate-others ps)]) (acquired->jsexpr x))))

(define (ii->jsexpr i)
  (match-define [ii d1 d2 rails cards connections] i)
  (hash (DESTINATION 1) (destination->jsexpr d1)
        (DESTINATION 2) (destination->jsexpr d2)
        RAILS           rails
        CARDS           cards
        CONNECTIONS     (acquired->jsexpr connections)))

(define (acquired->jsexpr c0)
  (for/list ([c (in-set c0)])
    (match-define [list cities color seg#] c)
    (match-define [list city1 city2] (set->list cities))
    (if (symbol<? city1 city2)
        (list city1 city2 (~a color) seg#)
        (list (~a city2) (~a city1) (~a color) seg#))))

(define (destination->jsexpr d)
  (match-define [list city1 city2] (set->list d))
  (if (symbol<? city1 city2) (list city1 city2) (list (~a city2) (~a city1))))

;                                                                                          
;                                                                                          
;        ;                                     ;             ;;;       ;                   
;        ;                                                     ;                           
;        ;                                                     ;                           
;    ;;; ;   ;;;;    ;;;;    ;;;;    ;;;;    ;;;      ;;;      ;     ;;;    ;;;;;;   ;;;;  
;   ;;  ;;  ;    ;  ;    ;  ;    ;   ;;  ;     ;     ;   ;     ;       ;        ;;  ;    ; 
;   ;    ;  ;;;;;;  ;       ;;;;;;   ;         ;         ;     ;       ;       ;;   ;;;;;; 
;   ;    ;  ;        ;;;;   ;        ;         ;     ;;;;;     ;       ;      ;;    ;      
;   ;    ;  ;            ;  ;        ;         ;    ;    ;     ;       ;     ;;     ;      
;   ;;  ;;  ;;   ;  ;    ;  ;;   ;   ;         ;    ;   ;;     ;       ;    ;;      ;;   ; 
;    ;;; ;   ;;;;;   ;;;;    ;;;;;   ;       ;;;;;   ;;; ;      ;;;  ;;;;;  ;;;;;;   ;;;;; 
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          

(define (parse-state [vgraph #false])
  (define j (read-message))
  (cond
    [(eof-object? j) #false]
    [(and (string? j) (regexp-match #px"ERR" j)) #false]
    [else (parse-proper j vgraph)]))

(define (parse-proper j vgraph)
  (define cities (and vgraph (graph-cities vgraph)))
  (define conns  (and vgraph (graph-connections vgraph)))
  (let/ec k
    (define (return s)
      (writeln s (current-error-port))
      (k #false))
    (match j
      [(hash-table ((? (curry eq? THIS)) this) ((? (curry eq? CONNECTIONS)) (list c ...)))
       (pstate (parse-this return this cities conns) (map (parse-acquired return cities conns) c))]
      [_ (return "not a state object (with the two required fields)")])))

(define (parse-this return j cities conns)
  (match j
    [(hash-table ((? (curry eq? (DESTINATION 1))) d1)
                 ((? (curry eq? (DESTINATION 2))) d2)
                 ((? (curry eq? RAILS)) (? natural? rails))
                 ((? (curry eq? CARDS)) cd)
                 ((? (curry eq? CONNECTIONS)) cs))
     (ii (parse-destination return d1 cities)
         (parse-destination return d2 cities)
         rails
         (parse-cards return cd)
         ((parse-acquired return cities conns) cs))]
    [_ (return "not a player object (with the six required fields)")]))

(define (parse-destination return j cities)
  (match j
    [(list (? city? city1) (? city? city2)) (2cities city1 city2 return cities)]
    [_ (return "not a destination array")]))

(define (parse-cards return j)
  (unless (hash? j) (return "not a card object"))
  (for/hash ([(c s) j])
    (unless (color? (~a c)) (return "not a color (in the domain of a card object)"))
    (unless (natural? s) (return "not a segment length (in the domain of a card object)"))
    (values c s)))

(define ((parse-acquired return cities conns) j)
  (for/set ([x j])
    (match x
      [(list (? city? city1) (? city? city2) (? color? c) (? seg#? s))
       (define candidate (list (2cities city1 city2 return cities) (string->symbol c) s))
       (if (or (boolean? conns) (set-member? conns candidate))
           candidate
           (return "non-existent connection"))]
      [_ (return "not a connection array (with 2 cities, a color, and a segment length")])))

(define (2cities city1 city2 return cities)
  (define c1 (string->symbol city1))
  (define c2 (string->symbol city2))
  (if (or (boolean? cities) (and (member c1 cities) (member c2 cities)))
      (set c1 c2)
      (return (~a "not cities: " city1 "::" cities))))

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

  (define-syntax-rule (dev-null e) (parameterize ([current-error-port (open-output-string)]) e))
  
  (define (->string g [vg #false])
    #;dev-null (with-input-from-string (jsexpr->string (pstate->jsexpr g)) (λ () (parse-state vg))))
  
  (check-equal? (->string pstate1) pstate1)
  (check-equal? (->string pstate1 vtriangle) pstate1)

  (check-false (with-input-from-file "state-serialize.rkt" parse-state) "bad file format")
  
  (check-false (with-input-from-string "" parse-state) "eof")


  (define A (acquired->jsexpr (all-available-connections vtriangle pstate1)))
  (define B (sort (sort A string<? #:key first) string<? #:key second))

  (define (<-string A)
    (with-input-from-string (jsexpr->string A)
      (compose (parse-acquired values #false #false) read-message)))

  (check-false (equal? A B))
  (check-equal? (<-string A) (<-string B)))


  