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

#; {GameMap -> Boolean}
;; GURANATEE All connections between two cities A and B are specified in only
;; one hash entry (either from A to B or B to A). Use string<? to make this work.
(define (guarantee serialized-graph)
  (define serialized-graph-proper (hash-ref serialized-graph CONNECTIONS))
  (define dom (names serialized-graph-proper))
  (define rng (map (λ (d) (names (hash-ref serialized-graph-proper d))) dom))
  (for/and ([d dom] [r rng])
    (andmap (λ (r) (symbol<? d r)) r)))

#; {[Hashof Symbol X] -> [Listof Symbol]}
(define (names h) (map car (hash->list h)))

(require (only-in json jsexpr?))

(provide
 URL
 CITIES
 WIDTH 
 HEIGHT
 CONNECTIONS
 
 (contract-out
  ;; extract VGraph from JSexpr on STDIN, #false otherwise
  [parse-game-map   (-> (or/c #false game-map?))]
  [parse-map        (-> jsexpr? (or/c #false game-map?))]
  
  [game-map->jsexpr (-> game-map? (and/c jsexpr? guarantee))]))

(module+ examples
  (provide vtriangle-serialized))

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
(require Trains/Common/map)
(require Trains/Lib/get-data-url)
(require Trains/Lib/get-image-from-url)
(require SwDev/Testing/communication)
(require (prefix-in htdp: 2htdp/image))
(require net/url)

(module+ examples
  (require (submod Trains/Common/map examples)))

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require (submod Trains/Common/map examples))
  (require json)
  (require rackunit))

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

(define URL    'url)

(define CITIES 'cities)
(define WIDTH  'width)
(define HEIGHT 'height)
(define CONNECTIONS 'connections)
(define URI    'URI)

#; {type Graph  = [Hash [WIDTH M] [HEIGHT N] [CITIES Cities] [CONNECTIONS JGraph]]}
#; {type Cities = [Listof [List String [List N N]]]}
#; {type JGraph = [Hashof Symbol JSlice]}
#; {type JSlice = [Hashof Symbol JColor]}
#; {type JColor = [Hashof Color Natural]}
;; The hash table maps city names to city names, which are mapped to colors
;; and those map to the number of segments. All connections are bi-directional.

(module+ examples
  
  (define vtriangle-serialized
    (hash CITIES      (map (λ (x) (cons (~a (first x)) (rest x))) triangle-nod*)
          ;; can this manual conversion be eliminated? 
          CONNECTIONS (hash 'Boston  (hash 'Orlando (hash 'green 5
                                                          'white 3)
                                           'Seattle  (hash 'green 4
                                                           'red 3))
                            'Orlando (hash 'Seattle  (hash 'blue 5))
                            'Seattle (hash))
          HEIGHT      MAX-WIDTH
          WIDTH       MAX-WIDTH)))

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
(define (game-map->jsexpr g)
  (cond
    [(game-map-png g)
     =>
     (λ (img)
       (hash URI (bytes->string/utf-8 (image->data-url img))
             CITIES (map (lambda (x) (cons (~a (first x)) (rest x))) (game-map-locations g))
             CONNECTIONS (graph->jsexpr g)))]
    [else 
     (hash WIDTH  (game-map-width g)
           HEIGHT (game-map-height g)
           CITIES (map (lambda (x) (cons (~a (first x)) (rest x))) (game-map-locations g))
           CONNECTIONS (graph->jsexpr g))]))

#; {Graph -> JGraph}
(define (graph->jsexpr graph)
  (for/hash ([c (game-map-cities graph)])
    (values c (to* graph c))))

#; {Graph City -> JSlice}
(define (to* graph city)
  (for*/hash ([connection* (group-by first (game-map-connections graph city))]
              [next-city   (in-value (first (first connection*)))]
              #:when (symbol<? city next-city))
    (define color+seg# (map rest connection*))
    (values next-city (connected-via color+seg#))))

#; {[Listof Connection] -> JColors}
(define (connected-via connection*)
  (for/hash ([c (group-by first connection*)])
    (values (first (first c)) (second (first c)))))

;                                                                               
;       ;                                                                       
;       ;                                 ;          ;;;       ;                
;       ;                                              ;                        
;    ;;;;   ;;;    ;;;    ;;;    ;;;;   ;;;   ;;;;     ;     ;;;   ;;;;;   ;;;  
;   ;; ;;  ;;  ;  ;   ;  ;;  ;   ;;  ;    ;       ;    ;       ;       ;  ;;  ; 
;   ;   ;  ;   ;; ;      ;   ;;  ;        ;       ;    ;       ;      ;   ;   ;;
;   ;   ;  ;;;;;;  ;;;   ;;;;;;  ;        ;    ;;;;    ;       ;     ;    ;;;;;;
;   ;   ;  ;          ;  ;       ;        ;   ;   ;    ;       ;    ;     ;     
;   ;; ;;  ;      ;   ;  ;       ;        ;   ;   ;    ;       ;   ;      ;     
;    ;;;;   ;;;;   ;;;    ;;;;   ;      ;;;;;  ;;;;     ;;   ;;;;; ;;;;;   ;;;; 
;                                                                               
;                                                                               
;                                                                               

#; {-> (U False VGraph)}
;; extract VGraph from JSexpr on STDIN, #false otherwise 
(define (parse-game-map)
  (define j (read-message))
  (cond
    [(eof-object? j) #false]
    [(and (string? j) (regexp-match #px"ERR" j)) #false]
    [else (parse-map j)]))

#; {JSexpr -> (U False VGraph)}
;; extract width, height, and list of nodes from JSexpr, #false otherwise 
(define (parse-map j)
  (let/ec k
    (define (return x)
      (displayln x (current-error-port))
      (k #false))
    (match j
      [(hash-table ((? (curry eq? WIDTH)) (? width? w))
                   ((? (curry eq? HEIGHT)) (? height? h))
                   ((? (curry eq? CITIES)) c)
                   ((? (curry eq? CONNECTIONS)) s))
       (define cities (map (parse-city w h return) c))
       (define city-names (map first cities))
       (unless (= (set-count (apply set city-names)) (length city-names))
         (return "duplicate city name"))
       (define city-locs  (map second cities))
       (unless (= (set-count (apply set city-locs)) (length city-locs))
         (return "two cities with identical location"))
       (define connections (parse-connections s city-names return))
       (construct-game-map w h cities connections)]
      [(hash-table ((? (curry eq? URI))         (? string? uri))
                   ((? (curry eq? CITIES))      c)
                   ((? (curry eq? CONNECTIONS)) s))

       (define the-map
         (with-handlers ([exn:fail? (λ (xn) (return (exn-message xn)))])
           (define uri-as-bytes (string->bytes/utf-8 uri))
           (cond
             [(is-data-url? uri-as-bytes) (extract-image uri-as-bytes)]
             [else (png-from-url uri)])))

       (define w (htdp:image-width the-map))
       (define h (htdp:image-height the-map))

       (define cities (map (parse-city w h return) c))
       (define city-names (map first cities))
       (unless (= (set-count (apply set city-names)) (length city-names))
         (return "duplicate city name"))
       (define city-locs  (map second cities))
       (unless (= (set-count (apply set city-locs)) (length city-locs))
         (return "two cities with identical location"))
       (define connections (parse-connections s city-names return))
       (construct-game-map w h cities connections #:map the-map)]
      [_ (return "not a map object (with the four required fields)")])))

#; {N N [Boolean -> Empty] -> JSexpr -> Node}
(define ((parse-city w h return) j)
  (match j
    [(list (? city? n)
           (list (and (? natural? x) (? (λ (y) (<= 0 y w))))
                 (and (? natural? y) (? (λ (y) (<= 0 y h))))))
     (list (string->symbol n) (list x y))]
    [_ (return (~e "not a proper city specification" j))]))

#; {type LConnection = [List Symbol Symbol ColorSymbol Seq#]}

#; {JSexpr [Listof Symbol] [Boolean -> Emtpy] -> [Listof LConnection]}
(define (parse-connections j cities return)
  (unless (hash? j) (return "not a connection object"))
  (for/fold ([r '()]) ([(from c*) j])
    (unless (member from cities) (return "not a city in the domain of the connection object"))
    (append r (map (λ (x) (cons from x)) (parse-1-connection c* cities return)))))

#; {JSexpr [Listof Symbol] [Boolean -> Emtpy] -> [Listof [List Symbol ColorSymbol Seg#]]}
(define (parse-1-connection j cities return)
  (unless (hash? j) (return "not a connection object"))
  (for/fold ([r '()]) ([(to c*) j])
    (unless (member to cities) (return "not a city in the range of the connection object"))
    (append r (map (λ (x) (cons to x)) (parse-edges c* return)))))

#; {JSexpr [Boolean -> Emtpy] -> [Listof [List ColorSymbol Seg#]]}
(define (parse-edges j return)
  (unless (hash? j) (return "not an edge object"))
  (for/list ([(color seg#) j])
    (unless (color? color) (return "not a color"))
    (unless (seg#? seg#) (return "not a segment length"))
    (list color seg#)))

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

(module+ test ;; serialization 
  (check-equal? (game-map->jsexpr vtriangle) vtriangle-serialized  "triangle"))

(module+ test ;; deserialization 

  (define-syntax-rule (dev-null e)
    (parameterize ([current-error-port (open-output-string)]) e))
  
  (define-syntax-rule (->vgraph g)
    (dev-null (with-input-from-string (jsexpr->string (game-map->jsexpr g)) parse-game-map)))
  
  (define example1 `([A [1 1]] (B [2 2])))
  (define connect1 '[[A B red 3]])
  (define graph1  [construct-game-map MAX-WIDTH MAX-WIDTH example1 connect1])
  
  (check-equal? (parse-map (game-map->jsexpr graph1)) graph1 "parse map")
  (check-equal? (->vgraph graph1) graph1 "parse")
 
  (define example2 `([A%D [1 1]] (B [2 2])))
  (check-exn exn:fail:contract?
             (λ () (->vgraph [construct-game-map MAX-WIDTH MAX-WIDTH example2 connect1]))
             "bad city")

  (define connect4 '[[A B red 9]])
  (check-exn exn:fail:contract?
             (λ () (->vgraph [construct-game-map MAX-WIDTH MAX-WIDTH example1 connect4]))
             "fail a cont")
  
  (define example3 `([A [1 1]] (B [2 2]) (A [3 3])))
  (define graph6 [construct-game-map MAX-WIDTH MAX-WIDTH example3 connect1])
  (check-false (->vgraph graph6) "duplicated city")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; invalid but well-formed JSON
  
  (define (->string g msg)
    (check-false (dev-null (with-input-from-string (jsexpr->string g) parse-game-map)) msg))

  (define cities1 '[["A" [1 1]] ["B" [2 2]]])

  (define gm3 (hash 'width "A" 'height 0 'connections #hash() 'cities '[]))
  (->string gm3 "bad width")

  (define gm4 (hash 'width MAX-WIDTH 'height MAX-WIDTH 'connections '() 'cities '[]))
  (->string gm4 "bad target connection")

  (define gm* (hash 'width MAX-WIDTH 'height MAX-WIDTH 'cities cities1))
  (define gm5 (hash-set  gm* 'connections (hash 'A '())))
  (->string gm5 "bad color connection")
  
  (define gm6 (hash-set gm* 'connections (hash 'A (hash 'B '[]))))
  (->string gm6 "bad length connection")
  
  (define gm7 (hash-set gm* 'connections (hash 'A (hash 'C '[]))))
  (->string gm7 "bad city destination")

  (define gm8 (hash-set gm*  'connections (hash 'C (hash 'B '[]))))
  (->string gm8 "bad city origination")
  
  (define gm9 (hash-set (hash-set gm* 'connections (hash)) 'cities '[["a" [1 1]] ["B" [1 1]]]))
  (->string gm9 "identical locations")

  (check-false (with-input-from-file "map-serialize.rkt" parse-game-map) "bad file format")
  
  (check-false (with-input-from-string "" parse-game-map) "eof"))

(module+ test
  (require Trains/Lib/get-image-from-url)
  (require (prefix-in htdp: 2htdp/image))
  (define uri-for-standadrd-map "/Users/matthias/Courses/21SwDev/Source/Images/map.png")
  (define png (htdp:scale .8 (png-from-url uri-for-standadrd-map)))
  (define png-width (htdp:image-width png))
  (define png-height (htdp:image-height png))
  (define graph2 [construct-game-map png-width png-height  example1 connect1 #:map png])
  (check-equal? (->vgraph graph2) graph2 "parse"))

(module+ examples
  (define vrectangle-serialized (game-map->jsexpr vrectangle))
  (provide vrectangle-serialized))
