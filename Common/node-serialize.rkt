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
      [(hash-table ('width w) ('height h) ('cities c) ('connections s))
       (define cities (map (parse-city w h return) c))
       (define connections (map (parse-connection (map node-name cities) return) s))
       (list w h cities connections)]
      [_ #false])))

#; {N N [Boolean -> Empty] -> JSexpr -> Nod*}
(define ((parse-city w h return) j)
  (match j
    [(list (? string? n)
           (list (and (? natural? x) (? (λ (x) (<= 0 x w))))
                 (and (? natural? y) (? (λ (y) (<= 0 y h))))))
     (node n (cord x y))]
    [_ (return #false)]))

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
  (define BACKGROUND (rectangle 10 10 'solid 'red))

  (define example `(,[node "A" [cord 1 1]] ,(node "B" [cord 2 2])))
  (check-equal? (caddr (parse-map (nodes->jsexpr example '[] BACKGROUND))) example "inverse")
  
  (check-false (with-input-from-file "node-serialize.rkt" parse) #false))