#lang racket/gui

;; a simple map editor

(provide
 #; {-> VGraph}
 ;; this simple map editor produces a visual graph (board) representation 
 ;; nodes can be added but not deleted (yet)
 ;; connections are added via a separate selection-based window 

 ;; TODO initialize it with a Board representation  
 map-editor)

(module+ homework
  (provide
   node
   cord
   #; {Connection* Nodes Image -> Image}
   draw-connections
   #; {Nodes Image -> Image}
   draw-nodes))

;; ---------------------------------------------------------------------------------------------------
(require Trains/Lib/image)
(require Trains/Editor/connections-editor)
(require Trains/Common/map)
(require Trains/Common/basic-constants)
(require 2htdp/image)
(require 2htdp/universe)
(require SwDev/Testing/communication)
(require racket/runtime-path)
(require (prefix-in p: pict))

;; ---------------------------------------------------------------------------------------------------
(define-runtime-path MAP "../Resources/map.png")
(define BACKG  (bitmap/file MAP))
(define CITY   (circle 10 'solid 'red))
(define BCOLOR 'black)
(define FSIZE  22)
(define FCOLOR 'white)
(define CITY?   "Do you want a city here?")
(define CANCEL? "Cancel")
(define NAME    "Enter the city's name")

#; {-> JSexpr}
;; pop up map editor
;; write the nodes out to STDOUT as JSON
(define (map-editor)
  (define-values (nod0 connections0 background)
    (match #false ; (parse) 
      [(? boolean? nod0) (values '[] '[] BACKG)]
      [(list w h cities connections) (values cities connections (rectangle w h 'solid BCOLOR))]))
  (match-define (list nod* connections) (edit-graph nod0 connections0 background))
  (construct-visual-graph (image-width background) (image-height background) nod* connections))

;; ---------------------------------------------------------------------------------------------------
#; {Nod* Connectiion* Image -> Nod*}
(define (edit-graph nod0 connections0 backg)
  (define connection (new connection% [connections connections0] [x0 (+ (image-width backg) 55)]))
  (define nodes
    (big-bang nod0
      [to-draw (draw-graph connection backg)]
      [on-tick values] ;; this is just triggers a regular update of the image w/ new connections 
      [on-mouse (add-node connection)]))
  (list nodes (send connection view)))

(define connection%
  (class object% (init-field connections) (init x0)
    (super-new)

    (define manage
      (manage-connections
       #:connections0 connections #:x x0
       (λ (+or- h)
         #; {(U '+ '-) [Hash FROM TO SEG# COLOR] -> Void}
         (define c (list (hash-ref h From) (hash-ref h To) (hash-ref h Color) (hash-ref h Seg#)))
         (set! connections (if (eq? +or- '+) (cons c connections) (remove c connections))))))
    
    (define/public (add loc) (manage loc))
    (define/public (view) connections)))

;; ---------------------------------------------------------------------------------------------------
;; adding nodes 
 
#; {[Instance Connection%] -> Nod* N N MouseEvent -> Nod*}
(define ((add-node connection) nod* x y me)
  (cond
    [(not (mouse=? "button-down" me)) nod*]
    [(eq? 'no (message-box/custom "" "" CITY? CANCEL? #f #f '[number-order default=1])) nod*]
    [else
     (define name (get-text-from-user NAME ""))
     (cond
       [(and (city? name) (not (member name (map node-name nod*))))
        (send connection add (for/list ([to nod*]) (list name (node-name to))))
        (cons (node name (cord x y)) nod*)]
       [else nod*])]))

;; ---------------------------------------------------------------------------------------------------
;; drawing 
#; {I[Instance Connection%] mage -> Nod* -> Image}
(define ((draw-graph connection background) nodes)
  (define +nodes (draw-nodes nodes background))
  (define +conns (draw-connections (send connection view) nodes +nodes))
  +conns)

#; {Nod* Image -> Image}
(define (draw-nodes nodes background)
  (for/fold ([image background]) ([n nodes])
    (match-define [node name [cord x y]] n)
    (define +city (place-image CITY x y image))
    (define +name (place-image (text name FSIZE FCOLOR) x y +city))
    +name))

#; {Connection* Nod* Image -> Image}
(define (draw-connections edges nodes background)
  (for/fold ([image background]) ([edge-set (group-by (λ (x) (take x 2)) edges)])
    (for/fold ([image image]) ([x edge-set][j (in-naturals)])
      (define i (* 5 j))
      (match-define [list from to c s] x)
      (match-define [cord x1 y1] (lookup from nodes))
      (match-define [cord x2 y2] (lookup to nodes))
      (add-segments image s (+ x1 i) y1 (+ x2 i) y2 c BCOLOR))))

#; {String Nod* -> Cord}
(define (lookup name nodes)
  (node-posn (first (memf (λ (n) (equal? (node-name n) name)) nodes))))

;; ---------------------------------------------------------------------------------------------------
(module+ picts
  (define example
    (hash 'width 200
          'height 200
          'cities '[["A" [10 10]] ["B" [190 190]]]
          'connections '[["A" "B" "red" 3]]))
  (with-input-from-string (with-output-to-string (λ () (send-message example))) map-editor))
