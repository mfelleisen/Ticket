#lang racket

;; a bi-directional graph representation of the railroad map

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

 #; {type VGraph}
 #; {type Node}
 #; {N N Nod* [Listof [List Symbol Symbol ColorSymbol Seq#]] -> VGraph}
 ;; ASSUMPTION Nod* and the two city names are consistent 
 construct-visual-graph
 node
 cord
 node-name
 node-posn

 #; {VGraph -> N}
 graph-width
 #; {VGraph -> N}
 graph-height
 #; {VGraph -> [Listof [List String N N]]}
 graph-locations

 #; {type Path        = Connection*}
 #; {type Connection* = [Listof Connection]}
 #; {type Connection}
 #; {Graph City City Connection*  -> [Listof Path]}
 #; (all-paths A B blocked)
 ;; produces a list of all open paths from `A` to `B`
 ;; with the optional argument 
 all-paths

 #; {Graph City -> Connection*}
 graph-connections
 
 #; {Graph -> [Listof City]}
 graph-cities
 
 #; {Connection* -> [List Color Seg#]}
 to-color+seg#
 
 to-city)

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

(module+ examples
  (provide vtriangle triangle-source triangle bad))

(module+ test
  (require (submod ".." examples))
  (require rackunit))

;                                                                          
;                                                                          
;        ;                                                                 
;        ;            ;                                                    
;        ;            ;                                                    
;    ;;; ;    ;;;   ;;;;;;    ;;;            ;;;;    ;;;;   ; ;;;          
;   ;;  ;;   ;   ;    ;      ;   ;           ;;  ;  ;    ;  ;;  ;;         
;   ;    ;       ;    ;          ;           ;      ;;;;;;  ;    ;         
;   ;    ;   ;;;;;    ;      ;;;;;           ;      ;       ;    ;         
;   ;    ;  ;    ;    ;     ;    ;           ;      ;       ;    ;    ;;   
;   ;;  ;;  ;   ;;    ;     ;   ;;           ;      ;;   ;  ;;  ;;    ;;   
;    ;;; ;   ;;; ;     ;;;   ;;; ;           ;       ;;;;;  ; ;;;     ;;   
;                                                           ;              
;                                                           ;              
;                                                           ;              
;                                                                          

;; THE VISUAL ELEMENTS 
(struct visual-graph [width height cities graph] #:transparent)

(define graph-width visual-graph-width)
(define graph-height visual-graph-height)
(define (graph-locations g)
  (for/list ([n (visual-graph-cities g)])
    `[,(~a (node-name n)) ,(rest (vector->list (struct->vector (node-posn n))))]))

#; {type VGraph = (visual-graph N N Nod* Graph)}

(struct node [name posn] #:prefab)
(struct cord [x y] #:prefab)
#; {type Nod* = [Listof Node]}
#; {type Node = (node Symbol Cord)}
#; {type Cord = (cord N N)}

;; THE GRAPH STRUCTURE 
#; {type Graph = [Hashof Symbol Connection*]}
;; maps city to all existing connections

(struct to [city color seg#] #:transparent)
#; {type Connection = (to City Color Seg#)}
#; {type City       = Symbol}
;; -- target city, its connection color, and the number of segments
;; ASSUMPTIONS
;; -- cities are consistently named (keep separate city to posn map for drawing)
;; -- connection are two-ways (undirected)

(module+ examples
  (define triangle-source
    '[[Seattle Boston red 3]
      [Seattle Boston green 4]
      [Seattle Orlando blue 5]
      [Orlando Boston white 3]
      [Orlando Boston green 5]])
  
  (define triangle
    [hash 'Seattle `[,[to 'Boston 'red 3]
                     ,[to 'Boston 'green 4]
                     ,[to 'Orlando 'blue 5]]
          'Orlando `[,[to 'Boston 'white 3]
                     ,[to 'Boston 'green 5]
                     ,[to 'Seattle 'blue 5]]
          'Boston  `[,[to 'Orlando 'white 3]
                     ,[to 'Orlando 'green 5]
                     ,[to 'Seattle 'red 3]
                     ,[to 'Seattle 'green 4]]])

  (define bad
    [hash 'Seattle `[,[to 'Boston 'red 3]
                     ,[to 'Boston 'green 4]
                     ,[to 'Orlanod 'blue 5]]
          'Orlando `[,[to 'Boston 'white 3]
                     ,[to 'Boston 'green 5]
                     ,[to 'Seattle 'blue 5]]
          'Boston  `[,[to 'Orlando 'white 3]
                     ,[to 'Orlando 'green 5]
                     ,[to 'Seattle 'red 3]
                     ,[to 'Seattle 'green 4]]]))

;                                                                                                  
;                                                                                                  
;                                                                              ;                   
;                                     ;                               ;                            
;                                     ;                               ;                            
;     ;;;    ;;;;   ; ;;;    ;;;;   ;;;;;;   ;;;;   ;    ;    ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;  
;    ;   ;  ;;  ;;  ;;   ;  ;    ;    ;      ;;  ;  ;    ;   ;   ;    ;        ;    ;;  ;;  ;;   ; 
;   ;       ;    ;  ;    ;  ;         ;      ;      ;    ;  ;         ;        ;    ;    ;  ;    ; 
;   ;       ;    ;  ;    ;   ;;;;     ;      ;      ;    ;  ;         ;        ;    ;    ;  ;    ; 
;   ;       ;    ;  ;    ;       ;    ;      ;      ;    ;  ;         ;        ;    ;    ;  ;    ; 
;    ;   ;  ;;  ;;  ;    ;  ;    ;    ;      ;      ;   ;;   ;   ;    ;        ;    ;;  ;;  ;    ; 
;     ;;;    ;;;;   ;    ;   ;;;;      ;;;   ;       ;;; ;    ;;;      ;;;   ;;;;;   ;;;;   ;    ; 
;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                                                                                  

(define (construct-visual-graph width height nod* c*)
  (visual-graph width height nod* (connections->graph c*)))

#; {[Listof [List Symbol Symvol ColorSymbol Seg#]] -> Graph}
(define (connections->graph c*)
  (define directed   (add-one-direction (hash) c*))
  (define flipped    (map (λ (x) (list* (second x) (first x) (cddr x))) c*))
  (define undirected (add-one-direction directed flipped))
  undirected)

#; {Graph [Listof [List Symbol Symvol ColorSymbol Seg#]] -> Graph}
(define (add-one-direction graph c*)
  (for/fold ([directed-graph graph]) ([c (group-by first c*)])
    (hash-update directed-graph  (caar c) (connect-to (map rest c)) '[])))
  
#; {[Listof [List Symbol ColorSymbol Seg#]] -> Connection* -> Connection*}
(define [(connect-to c*) old]
  (append old (map (λ (x) (apply to x)) c*)))

;                                                                          
;                                                                          
;            ;;;     ;;;                                    ;              
;              ;       ;                              ;     ;              
;              ;       ;                              ;     ;              
;     ;;;      ;       ;            ; ;;;     ;;;   ;;;;;;  ; ;;;    ;;;;  
;    ;   ;     ;       ;            ;;  ;;   ;   ;    ;     ;;   ;  ;    ; 
;        ;     ;       ;            ;    ;       ;    ;     ;    ;  ;      
;    ;;;;;     ;       ;            ;    ;   ;;;;;    ;     ;    ;   ;;;;  
;   ;    ;     ;       ;            ;    ;  ;    ;    ;     ;    ;       ; 
;   ;   ;;     ;       ;            ;;  ;;  ;   ;;    ;     ;    ;  ;    ; 
;    ;;; ;      ;;;     ;;;         ; ;;;    ;;; ;     ;;;  ;    ;   ;;;;  
;                                   ;                                      
;                                   ;                                      
;                                   ;                                      
;                                                                          

(define (all-paths graph start end [blocked '()])
  #; {Graph City City [Listof City] -> [Listof Path]}
  (define (all-paths graph start end been-there0)
    (cond
      [(member start been-there0) '[]]
      [else
       (define been-there (cons start been-there0))
       (define all-steps (lookup graph start blocked))
       (for/fold ([all-paths '()]) ([s all-steps])
         (if (symbol=? (to-city s) end)
             (cons (list s) all-paths)
             (append (add-step graph end s been-there) all-paths)))]))
  
  #; {Graph City Connection [Listof City] -> [Listof Path]}
  (define (add-step graph end 1step been-there)
    (match-define [to city color seg#] 1step)
    (map (λ (1path) (cons 1step 1path)) (all-paths graph city end been-there)))

  (all-paths graph start end '[]))

#; {Graph City -> Connection*}
(define (lookup graph city blocked)
  (define connections (hash-ref graph city #false))
  (when (boolean? connections)
    (displayln `[graph domain: ,(map car (hash->list graph))] (current-error-port))
    (error 'graph-lookup "can't happend, city not found ~e" city))
  (remove* blocked connections))

(define (graph-connections graph city) (hash-ref (visual-graph-graph graph) city '[]))

(define (graph-cities graph) (map car (hash->list (visual-graph-graph graph))))

(define (to-color+seg# connection*) (map (λ (x) (list (to-color x) (to-seg# x))) connection*))

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

  ;; ------------------------------------------------------------------------------------------------
  ;; tests for connections->graph
  
  (define (->set g)
    (map (λ (x) (list (first x) (apply set (rest x)))) (hash->list g)))

  (check-equal? (->set (connections->graph triangle-source)) (->set triangle))

  ;; ------------------------------------------------------------------------------------------------
  ;; tests for all-aths 

  (define-syntax-rule (dev-null e) (parameterize ([current-error-port (open-output-string)]) e))

  (check-equal? (apply set (all-paths triangle 'Seattle 'Boston))
                [set `[,[to 'Boston 'green 4]]
                     `[,[to 'Boston 'red 3]]
                     `[,[to 'Orlando 'blue 5] ,[to 'Boston 'green 5]]
                     `[,[to 'Orlando 'blue 5] ,[to 'Boston 'white 3]]])

  (check-equal? (apply set (all-paths triangle 'Seattle 'Boston `[,[to 'Orlando 'blue 5]]))
                [set `[,[to 'Boston 'green 4]]
                     `[,[to 'Boston 'red 3]]])

  (check-exn #px"can't" (λ () (dev-null (all-paths bad 'Seattle 'Boston)))))

(module+ examples
  (define nod*
    [list [node 'Boston [cord 0 0]]
          [node 'Seattle [cord 0 0]]
          [node 'Orlando [cord 0 0]]])

  (define vtriangle (visual-graph 0 0 nod* triangle)))