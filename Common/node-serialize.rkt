#lang racket

(provide
 #; {type Node = (node String Cord)}
 #; {type Cord = (cord N N)}
 #; {type Nod* = [Listof Node]}
 #; {type Connection* = [Listof Connection]}
 #; {type Connection = [List String String String N]}
 (struct-out node)
 (struct-out cord)
 nodes->jsexpr
 parse)

;; ---------------------------------------------------------------------------------------------------
(require Trains/Common/basic-constants)
(require SwDev/Testing/communication)
(require 2htdp/image)
(require json)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct node [name posn] #:prefab)
(struct cord [x y] #:prefab)

;; ---------------------------------------------------------------------------------------------------
;; serialize to JSexpr 

#; {Nod* Connection* Image -> MAP}
(define (nodes->jsexpr nod* connections background)
  (hash 'width (image-width background)
        'height (image-height background)
        'connections connections
        'cities
        (for/list ([n nod*])
          `[,(node-name n) ,(rest (vector->list (struct->vector (node-posn n))))])))
        
;; ---------------------------------------------------------------------------------------------------
;; parsing from JSON 


;; MAKE SURE THAT CITY NAMES AND CONNECTIONS DON'T GET DUPLICATED 


#; {-> (U False [List N N Nod* Connections])}
;; extract width, height, and list of nodes from MAP JSexpr on STDIN, #false otherwise 
(define (parse)
  (define j (read-message))
  (cond
    [(eof-object? j) #false]
    [(and (string? j) (regexp-match #px"ERR" j)) #false]
    [else (parse-map j)]))

#; {JSexpr -> (U False [Cons N [Cons N Nod*]]) : MAP}
;; extract width, height, and list of nodes from JSexpr, #false otherwise 
(define (parse-map j)
  (let/ec return
    (match j
      [(hash-table ('width (? width? w)) ('height h) ('cities c) ('connections s))
       (define cities (map (parse-city w h return) c))
       (define city-names (map node-name cities))
       (unless (= (set-count (apply set city-names)) (length city-names))
         (return #false))
       (define connections (map (parse-connection city-names return) s))

;;       MAKE SURE THAT NO CONNECTION IS REPEATED, EACH COLOR EXISTS AT MOST ONCE 
;;       Use string< to make sure no connection 

       (list w h cities connections)]
      [_ (return #false)])))

#; {N N [Boolean -> Empty] -> JSexpr -> Nod*}
(define ((parse-city w h return) j)
  (match j
    [(list (? city? n)
           (list (and (? natural? x) (? (λ (y) (<= 0 y w))))
                 (and (? natural? y) (? (λ (y) (<= 0 y h))))))
     (node n (cord x y))]
    [_ (return #false)]))

(require SwDev/Debugging/spy)

#; {[Listof String] [Boolean -> Emtpy] -> JSexpr -> Connection*}
(define ((parse-connection cities return) j)
  (match j
    [(list (and (? string? from)     (? (λ (x) (member x cities))))
           (and (? string? to)       (? (λ (x) (member x cities))))
           (and (? string? color)    (? (λ (x) (member x COLORS))))
           (and (? natural? seg#)    (? (λ (x) (member x SEG#)))))
     j]
    [_ (return #false)]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define BACKG (rectangle 10 10 'solid 'red))
  (define (->string x y)
    (with-input-from-string (jsexpr->string (nodes->jsexpr x y BACKG)) parse))

  (define example1 `(,[node "A" [cord 1 1]] ,(node "B" [cord 2 2])))
  (define connect1 '[["A" "B" "red" 3]])
  (define result1  `[10 10 ,example1 ,connect1])
  (check-equal? (parse-map (nodes->jsexpr example1 connect1 BACKG)) result1 "inverse")

  (check-equal? (->string example1 connect1) result1 "parse")
  (check-false (with-input-from-file "node-serialize.rkt" parse) "bad file format")
  (check-false (with-input-from-string "" parse) "eof")

  (define example2 `(,[node "A%D" [cord 1 1]] ,(node "B" [cord 2 2])))
  (check-false (->string example2 connect1) "bad city connection")

  (define x (hash-set (nodes->jsexpr example1 connect1 BACKG) 'width "A"))
  (check-false (with-input-from-string (jsexpr->string x) parse) "bad width")

  (define connect2 '[["A" "B" "red" 9]])
  (check-false (->string example1 connect2) "bad segments")

  (define connect3 '[["A" "B" "pink" 3]])
  (check-false (->string example1 connect3) "bad color")

  (define example3 `(,[node "A" [cord 1 1]] ,(node "B" [cord 2 2]) ,(node "A" [cord 3 3])))
  (check-false (->string example3 connect1) "duplicated city"))
