#lang racket

;; state de/serialization 


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

(provide

 DESTINATION
 THIS
 ACQUIRED
 RAILS
 CARDS

 pstate->jsexpr
 action->jsexpr
 destination->jsexpr
 destination-set->jsexpr
 
 parse-state parse-pstate
 parse-action 
 parse-acquired1
 #;(parse-destination return j cities gm)
 parse-destination

 parse-destination-set
 destination-set?)
 

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

(require Trains/Common/player-interface)
(require Trains/Common/connection)
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
(define ACQUIRED    'acquired)
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
  (hasheq THIS     (ii->jsexpr (pstate-I ps))
          ACQUIRED (for/list ([x (pstate-others ps)]) (acquired->jsexpr x))))

(define (ii->jsexpr i)
  (hasheq (DESTINATION 1) (destination->jsexpr (ii-destination1 i))
          (DESTINATION 2) (destination->jsexpr (ii-destination2 i))
          RAILS           (ii-rails i)
          CARDS           (ii-cards i)
          ACQUIRED     (acquired->jsexpr (ii-connections i))))

(define (acquired->jsexpr c0)
  (for/list ([c (in-set c0)])
    (connection-serialize c)))

(define (acquired1->jsexpr c)
  (match-define [list city1 city2 color seg#] c)
  (append (map ~a (list-cities city1 city2)) (list (~a color) seg#)))

(define (destination->jsexpr d) (map ~a (apply list-cities d)))

(define (action->jsexpr c0)
  (match c0
    [(? (curry equal? MORE)) c0]
    [c (connection-serialize c)]))

(define (destination-set->jsexpr s)
  (for/list ([x (in-set s)]) (map ~a (apply list-cities x))))

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

(define (read-and-parse-state [vgraph #false])
  (define j (read-message))
  (cond
    [(eof-object? j) #false]
    [(and (string? j) (regexp-match #px"ERR" j)) #false]
    [else (parse-state j vgraph)]))

(define (parse-state j (gm #false))
  (define cities (and gm (game-map-cities gm)))
  (define conns  (and gm (game-map-all-connections gm)))
  (let/ec k
    (define (return s)
      (writeln s (current-error-port))
      (k #false))
    (match j
      [(hash-table ((? (curry eq? THIS)) this) ((? (curry eq? ACQUIRED)) (list c ...)))
       (pstate (parse-this return this gm cities conns) (map (parse-acquired return cities conns) c))]
      [_ (return (format "not a state object (with the two required fields)\n~e" j))])))

(define parse-pstate parse-state)

(define (parse-this return j gm cities conns)
  (match j
    [(hash-table ((? (curry eq? (DESTINATION 1))) d1)
                 ((? (curry eq? (DESTINATION 2))) d2)
                 ((? (curry eq? RAILS)) (? natural? rails))
                 ((? (curry eq? CARDS)) cd)
                 ((? (curry eq? ACQUIRED)) cs))
     (ii (parse-destination d1 return cities gm)
         (parse-destination d2 return cities gm)
         rails
         (parse-cards return cd)
         ((parse-acquired return cities conns) cs)
         #false)]
    [_ (return "not a player object (with the six required fields)")]))

(define (parse-destination j (return values) (cities #false) (gm #false))
  (match j
    [(list (? city? from-c) (? city? to-c))
     (cond
       [(boolean? cities) (2cities from-c to-c return cities)]
       [else 
        (define d-candidate (2cities from-c to-c return cities))
        (define-values (from to) (apply values d-candidate))
        (if (or (not gm) (game-map-connected? gm from to))
            d-candidate
            (return "destinations aren't connected in the given map"))])]
    [_ (return "not a destination array")]))

(define (parse-cards return j)
  (unless (hash? j) (return "not a card object"))
  (for/hasheq ([(c s) j])
    (unless (color? c) (return "not a color (in the domain of a card object)"))
    (unless (natural? s) (return "not a count (in the domain of a card object)"))
    (values c s)))

(define ((parse-acquired return cities conns) j)
  (for/set ([x j])
    (parse-acquired1 x return cities conns)))

(define (parse-acquired1 x return (cities #false) (conns #false))
  (match x
    [(list (? city? city1) (? city? city2) (? color? c) (? seg#? s))
     (define candidate1 (append (2cities city1 city2 return cities) (list (string->symbol c) s)))
     (define candidate (apply connect candidate1))
     (if (or (boolean? conns) (set-member? conns candidate))
         candidate
         (return "non-existent connection"))]
    [_ (return (~s "not a connection array (with 2 cities, a color, and a segment length " x))]))

(define (2cities city1 city2 return cities)
  (define c1 (string->symbol city1))
  (define c2 (string->symbol city2))
  (if (or (boolean? cities) (and (member c1 cities) (member c2 cities)))
      (list c1 c2)
      (return (~a "not cities: "
                  city1
                  (member c1 cities)
                  " & "
                  city2
                  (member c2 cities) "::" cities ))))

(define (parse-action j)
  (let/ec done
    (define (return x)
      (displayln x (current-error-port))
      #false)
    (match j
      [(? (curry equal? MORE)) j]
      [a (parse-acquired1 j return)])))

(define (parse-destination-set j)
  (match j
    [`[,(and dest `(,[? city? from] ,(? city? to))) ...]
     (apply set (map parse-destination dest))]))

(define (destination-set? d)
  (and (set? d) (for/and ([x d]) (destination/c x))))


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

(module+ examples
  (require (submod Trains/Common/state examples))
  (provide pstate1-serialized)
  (define pstate1-serialized (pstate->jsexpr pstate1)))

(module+ test
  (require SwDev/Lib/should-be-racket)

  (define (->string g [vg #false])
    (define g-as-str (jsexpr->string (pstate->jsexpr g)))
    (dev/null (with-input-from-string g-as-str (λ () (read-and-parse-state vg)))))

  (check-false (->string pstate-play+ vtriangle++) "pstate's destination1 connects disjoint clique")

  (check-equal? (->string pstate1) pstate1)
  (check-equal? (->string pstate1 vtriangle) pstate1)

  (check-false (with-input-from-file "state-serialize.rkt" read-and-parse-state) "bad file format")
  
  (check-false (with-input-from-string "" read-and-parse-state) "eof")


  (define A (acquired->jsexpr (all-available-connections vtriangle pstate1)))
  (define B (sort (sort A string<? #:key first) string<? #:key second))
  
  (check-equal? A B))
