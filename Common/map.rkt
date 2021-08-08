#lang racket

;; a data representation of the railroad map

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

;; ---------------------------------------------------------------------------------------------------
(define lexi-cities/c (λ (x) (symbol<? (first x) (second x))))

(define destination/c (and/c (list/c symbol? symbol?) lexi-cities/c))

(define connection/c  (and/c [list/c symbol? symbol? color? seg#?] lexi-cities/c))
(define connection*/c [listof connection/c])
(define connection-from  first)
(define connection-to    second)
(define connection-color third)
(define connection-seg#  fourth)

(define (in-cities? nodes)
  (define cities (map first nodes))
  (λ (c*)
    (for/and ([c c*])
      (define r  (and (member (first c) cities) (member (second c) cities)))
      (unless r
        (writeln `[,(first c) or ,(second c) not cities: ,cities] (current-error-port)))
      r)))

(define (is-city? a-game-map)
  (define cities (map node-name (game-map-city-places a-game-map)))
  (λ (c) (member c cities)))

;; ---------------------------------------------------------------------------------------------------
(provide

 game-map?

 #; {type Destination  = [List City City]}
 ;; a destination card specifies two cities; there is guaranteed to be a path between them
 #; (destinaion? (list city1 city2)) #; impllies #; (symbol<? city1 city2)
 destination/c

 connection/c
 connection-from
 connection-to 
 connection-color
 connection-seg#
 
 (contract-out
  [construct-game-map
   (->i ([w width?]
         [h height?]
         [cities-and-places [listof [list/c symbol? [list/c natural? natural?]]]]
         [connections       [cities-and-places] (and/c connection*/c (in-cities? cities-and-places))])
        (r game-map?))]

  [game-map-width     (-> game-map? width?)]
  [game-map-height    (-> game-map? height?)]
  [game-map-cities    (-> game-map? [listof symbol?])]
  [game-map-locations (-> game-map? [listof [list/c symbol? (list/c natural? natural?)]])]

  [game-map-all-connections (-> game-map? (set/c (list/c symbol? symbol? color? seg#?)))]
  [game-map-connections     (-> game-map? symbol? (listof (list/c symbol? color? seg#?)))]
  
  [all-destinations
   ;; produces all destinations for the given graph (in lexicographically directed form)
   (-> game-map? (listof destination/c))]

  (all-paths
   ;; produces a list of all paths from `A` to `B` in the given `vgraph`
   ;; GUARANTEE start from the symbol<? of the two cities, reach the other one (paths are 2-dir)
   (->i ([g game-map?] [from (g) (is-city? g)] [to (g) (is-city? g)]) (r (listof connection*/c))))
  
  [all-possible-paths
   ;; produces a list of all paths in the given graph
   ;; GUARANTEE every path connects `A` and `B` such that `(symbol<? A B)` holds
   (-> game-map? (listof connection*/c))]))

(module+ examples
  (provide vrectangle)
  (provide vtriangle-paths vtriangle-dests vtriangle-conns vtriangle-boston-seattle)
  (provide vtriangle triangle-nod* triangle-source triangle))

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
(struct game-map [width height city-places graph] #:transparent)

(define (game-map-locations g)
  (for/list ([n (game-map-city-places g)])
    `[,(node-name n) ,(rest (vector->list (struct->vector (node-posn n))))]))

#; {type VGraph = (visual-graph N N Nod* Graph)}

(struct node [name posn] #:prefab)
(struct cord [x y] #:prefab)
#; {type Nod* = [Listof Node]}
#; {type Node = (node Symbol Cord)}
#; {type Cord = (cord N N)}

;; THE GRAPH STRUCTURE 
#; {type Graph = [Hashof Symbol Slice*]}
;; maps city to all existing Slices

(struct to [city color seg#] #:prefab)
#; {type Slice = (to Symbol Color Seg#)}
;; -- target city, its color, and the number of segments
;; ASSUMPTIONS
;; -- cities are consistently named (keep separate city to posn map for drawing)
;; -- `to`s are two-ways (undirected)

(module+ examples
  (define triangle-source
    '[[Orlando Seattle blue 5]
      [Boston Seattle red 3]
      [Boston Seattle green 4]
      [Boston Orlando white 3]
      [Boston Orlando green 5]])
  
  (define triangle
    [hash 'Orlando `[,[to 'Boston 'white 3]
                     ,[to 'Boston 'green 5]
                     ,[to 'Seattle 'blue 5]]          
          'Seattle `[,[to 'Orlando 'blue 5]
                     ,[to 'Boston 'red 3]
                     ,[to 'Boston 'green 4]]
          'Boston  `[,[to 'Seattle 'red 3]
                     ,[to 'Seattle 'green 4]
                     ,[to 'Orlando 'white 3]
                     ,[to 'Orlando 'green 5]]])

  (define simple-triangle
    [hash 'Orlando `[,[to 'Boston  'blue 3]
                     ,[to 'Seattle 'blue 3]]
          'Boston  `[,[to 'Seattle 'blue 3]
                     ,[to 'Orlando 'blue 3]]
          'Seattle `[,[to 'Boston  'blue 3]
                     ,[to 'Orlando 'blue 3]]])

  (provide simple-triangle-paths)
  (define simple-triangle-paths 
    '{;; Boston <-> Seattle
      ;; ------------------
      [(Boston Seattle blue 3) (Orlando Seattle blue 3)] 
      [(Boston Seattle blue 3)]
      ;; Boston <-> Orlando
      ;; ------------------
      [(Boston Orlando blue 3) (Orlando Seattle blue 3)]
      [(Boston Orlando blue 3)]
      ;; Orlando <-> Seattle
      ;; -------------------
      [(Orlando Seattle blue 3)]
      [(Boston Orlando blue 3) (Boston Seattle blue 3)]})
    
  (define triangle-nod*
    '[[Boston  [100 100]]
      [Seattle [200 20]]
      [Orlando [30 300]]])

  (define vtriangle (game-map MAX-WIDTH MAX-WIDTH (list->node triangle-nod*) triangle))
  (define striangle (game-map MAX-WIDTH MAX-WIDTH (list->node triangle-nod*) simple-triangle))
  
  (define vtriangle-boston-seattle (list 'Boston 'Seattle 'red 3))
  (define vtriangle-paths (all-possible-paths vtriangle))
  (define vtriangle-conns (set vtriangle-boston-seattle))
  (define vtriangle-dests
    (append (all-destinations vtriangle)
            '[[Boston SanDiego]
              [Orlando SanDiego]
              [Chicago SanDiego]
              [SanDiego Seattle]])))

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

(define (construct-game-map width height nod* c*)
  (game-map width height (list->node nod*) (connections->graph c*)))

#; {[Listof [List Symbol [List N N]]] -> [Listof Node]}
(define (list->node nod*)
  (map (λ (x) (node (first x) (apply cord (second x)))) nod*))

#; {[Listof [List Symbol Symvol ColorSymbol Seg#]] -> Graph}
(define (connections->graph c*)
  (let* ([graph (hash)]
         [graph (add-one-direction graph c*)]
         [graph (add-one-direction graph (flip-from-to c*))])
    graph))

#; {[Listof Connection] -> [Listof Connection]}
(define (flip-from-to c*)
  (map (λ (x) (list* (second x) (first x) (cddr x))) c*))

#; {[Hashof Symbol Slice] [Listof Connections] -> [Hashof Symbol Slice]}
(define (add-one-direction graph c*)
  (for*/fold ([directed-graph graph]) ([c (group-by connection-from c*)][from (in-value (caar c))])
    (hash-update directed-graph from (curry append (map (λ (x) (apply to (rest x))) c)) '[])))

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

#; {GameMap -> [Listof Destination]}
(define (all-destinations vgraph)
  (define graph  (game-map-graph vgraph))
  (define cities (game-map-cities vgraph))
  (for*/fold ([destinations '()]) ([from cities][to cities] #:when (symbol<? from to))
    (define are-there-any-paths (all-paths vgraph from to))
    (if are-there-any-paths (cons (list from to) destinations) destinations)))

(define (all-possible-paths vgraph)
  (define graph  (game-map-graph vgraph))
  (define cities (game-map-cities vgraph))
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
       (define all-steps  (hash-ref graph start '[]))
       (for/fold ([paths '()]) ([1step all-steps])
         (match-define [to city color seg#] 1step)
         (define adder (add-step start city color seg#))
         (cond
           [(symbol=? city the-end) (append (list (adder '[])) paths)]
           [else (append (map adder (all-paths city been-there)) paths)]))]))
  
  #; {City City Color Seg# -> [Path ->  Path]}
  (define (add-step from to color seg#)
    (define 1step (append (list-cities from to) (list color seg#)))
    (λ (path) (cons 1step path)))

  (match-define (list the-start the-end) (list-cities start end))
  (all-paths the-start '[]))

(define (game-map-connections gm city)
  (define connections (hash-ref (game-map-graph gm) city '[]))
  (map (λ (x) (rest (vector->list (struct->vector x)))) connections))

(define (game-map-all-connections graph)
  (for/fold ([s (set)]) ([from (game-map-cities graph)])
    (set-union
     s
     (for/set ([c (game-map-connections graph from)])
       (append (list-cities from (first c)) (rest c))))))

(define (game-map-cities graph) (map node-name (game-map-city-places graph)))

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

  (provide striangle)
  
  (define vrectangle
    (game-map
     1000
     800
     '(#s(node |San Diego| #s(cord 176 571))
       #s(node Orlando     #s(cord 715 528))
       #s(node Boston      #s(cord 793 201))
       #s(node NYC         #s(cord 693 301))
       #s(node Seattle     #s(cord 131 168)))
     '#hash((Boston      . (#s(to Seattle white 3) #s(to Orlando white 5) #s(to NYC white 3)))
            (NYC         . (#s(to Boston white 3)))
            (Orlando     . (#s(to |San Diego| blue 5)
                            #s(to |San Diego| red 5)))
            (|San Diego| . (#s(to Seattle white 4)))
            (Seattle     . ())))))

(module+ test

  (check-equal? (construct-game-map MAX-WIDTH MAX-WIDTH triangle-nod* triangle-source) vtriangle)


  ;; ------------------------------------------------------------------------------------------------
  ;; tests for connections->graph
  
  (define (->set g)
    (map (λ (x) (list (first x) (apply set (rest x)))) (hash->list g)))

  (check-equal? (->set (connections->graph triangle-source)) (->set triangle))

  ;; ------------------------------------------------------------------------------------------------
  ;; tests for all-aths 

  (define-syntax-rule (dev-null e) (parameterize ([current-error-port (open-output-string)]) e))
  
  (check-equal? (apply set (all-paths vtriangle 'Seattle 'Boston))
                [set `[[Boston Seattle green 4]]
                     `[[Boston Seattle red 3]]
                     `[[Boston Orlando green 5] [Orlando Seattle blue 5]]
                     `[[Boston Orlando white 3] [Orlando Seattle blue 5]]])

  (check-equal? (apply set (all-possible-paths striangle))
                (apply set simple-triangle-paths))

  (check-equal? 
   (apply set (all-possible-paths vtriangle))
   (set-union
    (apply set (all-paths vtriangle 'Boston 'Seattle))
    (apply set (all-paths vtriangle 'Boston 'Orlando))
    (apply set (all-paths vtriangle 'Orlando 'Seattle))))

  ;; -------------------------------------------------------------------------------------------------
  ;; for game-map-connections, all of them 
  (define symmetric
    (for/set ([x triangle-source])
      (append (list-cities (first x) (second x)) (cddr x))))
  
  (check-equal? (game-map-all-connections vtriangle) symmetric "game-map-connection all")

  (check-equal? (apply set (all-destinations vrectangle))
                (apply set '[[Boston |San Diego|]
                             [Boston Seattle]
                             [Boston Orlando]
                             [Boston NYC]
                             ; [NYC Boston]
                             [NYC |San Diego|]
                             [NYC Orlando]
                             [NYC Seattle]
                             [Orlando |San Diego|]
                             [Orlando Seattle]
                             [|San Diego| Seattle]])))
                  
