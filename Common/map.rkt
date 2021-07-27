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

(require Trains/Common/basic-constants)

(define connection  [list/c symbol? symbol? color? seg#?])
(define connection* [listof connection])

(define path-step/c [list/c (set/c symbol?) color? seg#?])
(define path/c      [listof path-step/c])

(define (in? nodes)
  (define cities (map node-name nodes))
  (λ (c*)
    (for/and ([c c*])
      (unless (and (member (first c) cities) (member (second c) cities))
        (displayln `[,(first c) or ,(second c) not cities: ,cities] (current-error-port))))))

(define (is-city? a-game-map)
  (define cities (map node-name (game-map-cities a-game-map)))
  (λ (c) (member c cities)))

(provide

 (struct-out node)
 (struct-out cord)

 game-map?

 (contract-out
  
  [construct-visual-graph
   (->i ([w width?]
         [h height?]
         [nodes [listof [struct/c node symbol? [struct/c cord natural? natural?]]]]
         [conns (nodes) (and/c connection* (in? nodes))]) 
        (r game-map?))]

  (all-paths
   ;; produces a list of all paths from `A` to `B` in the given `vgraph`
   ;; GUARANTEE start from the symbol<? of the two cities, reach the string>=? of the two 
   (->i ([g game-map?] [from (g) (is-city? g)] [to (g) (is-city? g)]) (r (listof path/c))))
  
  [all-possible-paths
   ;; produces a list of all paths in the given graph
   ;; GUARANTEE every path connects `A` and `B` such that `(symbol<? A B)` holds
   (-> game-map? (listof path/c))])
   
   
 #; {VGraph -> N}
 graph-width
 #; {VGraph -> N}
 graph-height
 #; {VGraph -> [Listof [List String N N]]}
 graph-locations

 #; {type Path        = Connection*}
 #; {type Connection* = [Listof Connection]}
 #; {type Connection}
 
 #; {Graph City -> Connection*}
 graph-connections
 
 #; {Graph -> [Listof City]}
 graph-cities

 graph-nodes
 
 #; {Connection* -> [List Color Seg#]}
 to-color+seg#
 
 to-city)

(module+ examples
  (provide vtriangle triangle-source triangle vbad))

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

(module+ test
  (require (submod ".."))
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
(struct game-map [width height cities graph] #:transparent)

(define graph-nodes game-map-cities)

(define graph-width game-map-width)
(define graph-height game-map-height)
(define (graph-locations g)
  (for/list ([n (game-map-cities g)])
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

(struct to [city color seg#] #:prefab)
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
                     ,[to 'Orlanod 'green 5]
                     ,[to 'Seattle 'red 3]
                     ,[to 'Seattle 'green 4]]])

  (define nod*
    [list [node 'Boston [cord 0 0]]
          [node 'Seattle [cord 0 0]]
          [node 'Orlando [cord 0 0]]])

  (define vbad (game-map 0 0 nod* bad))
  (define vtriangle (game-map 0 0 nod* triangle)))

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
  (game-map width height nod* (connections->graph c*)))

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

(define (all-possible-paths vgraph)
  (define graph  (game-map-graph vgraph))
  (define cities (graph-cities vgraph))
  (for*/fold ([paths '()]) ([from cities][to cities] #:when (symbol<? from to))
    (append paths (all-paths vgraph from to))))

(define (all-paths vgraph start end)
  (define graph (game-map-graph vgraph))
      
  #; {City [Listof City] -> [Listof Path]}
  ;; find all paths from `start` to `end`, unless `start` is in `been-there0`
  ;; ACCU `been-there0` is a list of all cities seen between `start0` and `start` in `graph`
  (define (all-paths start been-there0)
    (cond
      [(member start been-there0) '()]
      [else
       (define been-there (cons start been-there0))
       (define all-steps  (lookup graph start))
       (for/fold ([paths '()]) ([1step all-steps])
         (match-define [to city color seg#] 1step)
         (define adder (add-step start city color seg#))
         (cond
           [(symbol=? city the-end) (append (list (adder '[])) paths)]
           [else (append (map adder (all-paths city been-there)) paths)]))]))
  
  #; {City City Color Seg# -> [Path ->  Path]}
  (define (add-step from to color seg#)
    (define 1step (list (set from to) color seg#))
    (λ (path)
      (cons 1step path)))

  (define-values (the-start the-end) (if (symbol<? start end) (values start end) (values end start)))
  (all-paths the-start '[]))

#; {Graph City -> Connection*}
(define (lookup graph city)
  (define connections (hash-ref graph city #false))
  (when (boolean? connections)
    (displayln `[graph domain: ,(map car (hash->list graph))] (current-error-port))
    (error 'graph-lookup "can't happen, city not found ~e" city))
  connections)

(define (graph-connections graph [city #false])
  (if (symbol? city)
      (hash-ref (game-map-graph graph) city '[])
      (set-of-all-connections graph)))

(define (set-of-all-connections graph)
  (for/fold ([s (set)]) ([c (graph-cities graph)])
    (set-union
     s
     (for/set ([l (graph-connections graph c)])
       (list (set c (to-city l)) (to-color l) (to-seg# l))))))

(define (graph-cities graph) (map node-name (game-map-cities graph)))

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

(module+ examples ;; more

  (provide vrectangle)

  (define vrectangle
    (game-map
     1000
     800
     '(#s(node |San Diego| #s(cord 176 571))
       #s(node Orlando #s(cord 815 528))
       #s(node Boston #s(cord 893 201))
       #s(node Seattle #s(cord 131 168)))
     '#hash((Boston
             . (#s(to Seattle red 3) #s(to Orlando green 5)))
            (Orlando
             . (#s(to Seattle green 5) #s(to Boston green 5) #s(to |San Diego| green 5)))
            (|San Diego|
             . (#s(to Seattle blue 4) #s(to Orlando green 5)))
            (Seattle
             . (#s(to |San Diego| blue 4) #s(to Orlando green 5) #s(to Boston red 3)))))))

(module+ test

  ;; ------------------------------------------------------------------------------------------------
  ;; tests for connections->graph
  
  (define (->set g)
    (map (λ (x) (list (first x) (apply set (rest x)))) (hash->list g)))

  (check-equal? (->set (connections->graph triangle-source)) (->set triangle))

  ;; ------------------------------------------------------------------------------------------------
  ;; tests for all-aths 

  (define-syntax-rule (dev-null e) (parameterize ([current-error-port (open-output-string)]) e))
  
  (check-equal? (apply set (all-paths vtriangle 'Seattle 'Boston))
                [set `[[,(set 'Boston 'Seattle) green 4]]
                     `[[,(set 'Boston 'Seattle) red 3]]
                     `[[,(set 'Orlando 'Boston) green 5] [,(set 'Orlando 'Seattle) blue 5]]
                     `[[,(set 'Orlando 'Boston) white 3] [,(set 'Orlando 'Seattle) blue 5]]])
  
  (check-exn #px"can't" (λ () (dev-null (all-paths vbad 'Seattle 'Boston))))
  
  (check-equal? 
   (apply set (all-possible-paths vtriangle))
   (set-union
    (apply set (all-paths vtriangle 'Boston 'Seattle))
    (apply set (all-paths vtriangle 'Boston 'Orlando))
    (apply set (all-paths vtriangle 'Orlando 'Seattle))))

  ;; -------------------------------------------------------------------------------------------------
  ;; for graph-connections, all of them 
  (define symmetric
    (for/set ([x triangle-source])
      (cons (set (first x) (second x)) (cddr x))))
  
  (check-equal? (graph-connections vtriangle) symmetric "graph-connection all")

  (all-possible-paths vrectangle))