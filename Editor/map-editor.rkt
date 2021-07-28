#lang racket/gui

;; a simple map editor

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
 #; {-> VGraph}
 ;; this simple map editor produces a visual graph (board) representation 
 ;; nodes can be added but not deleted (yet)
 ;; connections are added via a separate selection-based window 
 map-editor)

(module+ homework
  (provide
   node cord
   
   #; {[Listof InternalConnection] Nodes Image -> Image}
   draw-connections
   
   #; {Nodes Image -> Image}
   draw-nodes))

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

(require Trains/Lib/image)
(require Trains/Editor/connections-editor)
(require Trains/Common/map)
(require Trains/Common/map-serialize)
(require Trains/Common/basic-constants)
(require (except-in 2htdp/image color?))
(require 2htdp/universe)
(require SwDev/Testing/communication)
(require racket/runtime-path)

(module+ test
  (require (submod Trains/Common/map-serialize examples))
  (require rackunit))

(module+ picts
  (require (submod Trains/Common/map-serialize examples)))

;                                                          
;                                                          
;                ;     ;               ;                   
;                ;     ;      ;        ;                   
;                ;            ;                            
;    ;;;;    ;;; ;   ;;;    ;;;;;;   ;;;    ; ;;;    ;;; ; 
;    ;  ;;   ;  ;;     ;      ;        ;    ;;   ;   ;  ;; 
;   ;    ;  ;    ;     ;      ;        ;    ;    ;  ;    ; 
;   ;;;;;;  ;    ;     ;      ;        ;    ;    ;  ;    ; 
;   ;       ;    ;     ;      ;        ;    ;    ;  ;    ; 
;   ;       ;    ;     ;      ;        ;    ;    ;  ;    ; 
;    ;       ;  ;;     ;      ;        ;    ;    ;   ;  ;; 
;    ;;;;;   ;;; ;  ;;;;;;;    ;;;  ;;;;;;; ;    ;   ;;; ; 
;                                                        ; 
;                                                    ;  ;; 
;                                                     ;;;  
;                                                          

(define-runtime-path MAP "../Resources/map.png")
(define BACKG  (scale .8 (bitmap/file MAP)))
(define CITY   (circle 10 'solid 'red))
(define BCOLOR 'black)
(define FSIZE  22)
(define FCOLOR 'white)
(define CITY?   "Do you want a city here?")
(define CANCEL? "Cancel")
(define NAME    "Enter the city's name")

#; {-> Map}
;; pop up map editor, wait for user to add cities to map & connections, construct map rep.
(define (map-editor)
  (define-values (cities0 connections0 background)
    (match (parse-game-map) 
      [(? boolean?)  (values '[] '[] BACKG)]
      [(? game-map? g)
       (define cities (externa->internal-cities (game-map-cities g)))
       (define conns  (externa->internal-connections (game-map-all-connections g)))         
       (values cities conns (rectangle (graph-width g) (graph-height g) 'solid BCOLOR))]))
  (match-define (list cities connections) (edit-graph cities0 connections0 background))
  (construct-game-map (image-width background) (image-height background) cities connections))

;                                                                                          
;                                                                                          
;      ;                                                               ;                   
;      ;              ;                                       ;        ;                   
;                     ;                                       ;                            
;    ;;;    ; ;;;   ;;;;;;   ;;;;    ;;;;;    ;;;     ;;;   ;;;;;;   ;;;    ; ;;;    ;;; ; 
;      ;    ;;   ;    ;      ;  ;;   ;;      ;   ;   ;   ;    ;        ;    ;;   ;   ;  ;; 
;      ;    ;    ;    ;     ;    ;   ;           ;  ;         ;        ;    ;    ;  ;    ; 
;      ;    ;    ;    ;     ;;;;;;   ;       ;;;;;  ;         ;        ;    ;    ;  ;    ; 
;      ;    ;    ;    ;     ;        ;      ;    ;  ;         ;        ;    ;    ;  ;    ; 
;      ;    ;    ;    ;     ;        ;      ;    ;  ;         ;        ;    ;    ;  ;    ; 
;      ;    ;    ;    ;      ;       ;      ;   ;;   ;   ;    ;        ;    ;    ;   ;  ;; 
;   ;;;;;;; ;    ;     ;;;   ;;;;;   ;       ;;; ;    ;;;      ;;;  ;;;;;;; ;    ;   ;;; ; 
;                                                                                        ; 
;                                                                                    ;  ;; 
;                                                                                     ;;;  
;                                                                                          

#; {type InternalConnection = [List String String Color Length]}
#; {type InternalCities     = [Lisof [node String [cord N N]]]}
;; drawing and presenting choices works with strings, not symbols

(define (externa->internal-connections externals)
  (for/list ([c (set->list externals)])
    (append (map ~a (set->list (first c))) (rest c))))

(define (internal->external-connections internal-connections)  
  (for/list ([c internal-connections])
    (append (map string->symbol (take c 2)) (drop c 2))))

(define (externa->internal-cities nodes)
  (for/list ([c nodes])
    (node (~a (node-name c)) (node-posn c))))

(define (internal->external-cities cities)
  (for/list ([n cities])
    (node (string->symbol (node-name n)) (node-posn n))))

#; {[Listof InternalCities]
    [Listof InternalConnection]
    Image -> [List [Listof InternalCities] [Listof InternalConnection]]}
(define (edit-graph nod0 connections0 backg)
  (define connection (new connection% [*connections connections0] [x0 (+ (image-width backg) 55)]))
  (define nodes (internal->external-cities (edit-with-big-bang nod0 connection backg)))
  (define conns (internal->external-connections (send connection view)))
  (send connection stop)
  (list nodes conns))

(define max-time (make-parameter 100000000)) ;; for testing
#; {[Listof InternalCities] [Listof InteranlConnections] Image -> [Listof InternalCities]}
(define (edit-with-big-bang nod0 connection backg)
  (big-bang nod0
    [to-draw (draw-graph connection backg)]
    [on-mouse (add-city connection)]
    ;; this just triggers a regular update of the image w/ new connections 
    [on-tick values 1 (max-time)]
    [close-on-stop #true]))

#; {[Instance Connection%] -> [Listof InternalCities] N N MouseEvent -> [Listof InternalCities]}
(define ((add-city connection) cities x y me)
  (cond
    [(not (mouse=? "button-down" me)) cities]
    [(eq? 'no (message-box/custom "" "" CITY? CANCEL? #f #f '[number-order default=1])) cities]
    [else
     (define name (get-text-from-user NAME ""))
     (cond
       [(and (city? name) (not (member name (map node-name cities))))
        (send connection add (for/list ([to cities]) (list name (node-name to))))
        (cons (node name (cord x y)) cities)]
       [else cities])]))

(define connection% ;; manage InternalConnections via edior and for later use 
  (class object% (init-field *connections) (init x0)
    (super-new)

    (define ct (make-custodian))
    (define/public (stop) (custodian-shutdown-all ct))
    
    (define manage
      (parameterize ([current-custodian ct])
        (parameterize ([current-eventspace (make-eventspace)])
          (define frame (new frame% [label "edit connections"] [width 300] [height 300]))
          (manage-connections
           #:frame frame
           #:connections0 *connections #:x x0
           (λ (+or- h)
             #; {(U '+ '-) [Hash FROM TO SEG# COLOR] -> Void}
             (define c (external->internal h))
             (set! *connections (if (eq? +or- '+) (cons c *connections) (remove c *connections))))))))

    (define/private (external->internal h)
      (list (hash-ref h From) (hash-ref h To) (hash-ref h Color) (hash-ref h Seg#)))
    
    (define/public (add loc) (manage loc))
    (define/public (view) *connections)))

;                                                          
;                                                          
;        ;                             ;                   
;        ;                             ;                   
;        ;                                                 
;    ;;; ;   ;;;;;    ;;;  ;      ;  ;;;    ; ;;;    ;;; ; 
;    ;  ;;   ;;      ;   ; ;      ;    ;    ;;   ;   ;  ;; 
;   ;    ;   ;           ; ;     ;     ;    ;    ;  ;    ; 
;   ;    ;   ;       ;;;;;  ; ;; ;     ;    ;    ;  ;    ; 
;   ;    ;   ;      ;    ;  ; ;; ;     ;    ;    ;  ;    ; 
;   ;    ;   ;      ;    ;  ;;;;;;     ;    ;    ;  ;    ; 
;    ;  ;;   ;      ;   ;;  ;;  ;;     ;    ;    ;   ;  ;; 
;    ;;; ;   ;       ;;; ;   ;  ;   ;;;;;;; ;    ;   ;;; ; 
;                                                        ; 
;                                                    ;  ;; 
;                                                     ;;;  
;                                                          

#; {I[Instance Connection%] mage -> [Listof InternalCities] -> Image}
(define ((draw-graph connection background) cities)
  (define +nodes (draw-nodes cities background))
  (define +conns (draw-connections (send connection view) cities +nodes))
  +conns)

#; {[Listof InternalCities] Image -> Image}
(define (draw-nodes cities background)
  (for/fold ([image background]) ([n cities])
    (match-define [node name [cord x y]] n)
    (define +city (place-image CITY x y image))
    (define +name (place-image (text name FSIZE FCOLOR) x y +city))
    +name))

#; {[Listof InternalConnection] [Listof InternalCities] Image -> Image}
(define (draw-connections edges cities background)
  (for/fold ([image background]) ([edge-set (group-by (λ (x) (take x 2)) edges)])
    (for/fold ([image image]) ([x edge-set][j (in-naturals)])
      (define i (* 5 j))
      (match-define [list from to c s] x)
      (match-define [cord x1 y1] (lookup from cities))
      (match-define [cord x2 y2] (lookup to cities))
      (add-segments image s (+ x1 i) y1 (+ x2 i) y2 c BCOLOR))))

#; {String [Listof InternalCities] -> Cord}
(define (lookup name cities)
  (node-posn (first (memf (λ (n) (equal? (node-name n) name)) cities))))

;                                          
;                                          
;                                          
;     ;                       ;            
;     ;                       ;            
;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;  
;     ;      ;  ;;  ;    ;    ;     ;    ; 
;     ;     ;    ;  ;         ;     ;      
;     ;     ;;;;;;  ;;;       ;     ;;;    
;     ;     ;          ;;;    ;        ;;; 
;     ;     ;            ;    ;          ; 
;     ;      ;      ;    ;    ;     ;    ; 
;      ;;;   ;;;;;   ;;;;      ;;;   ;;;;  
;                                          
;                                          
;                                          
;                                          

(module+ test
  (check-equal?
   (parameterize ((max-time 1))
     (game-map->jsexpr 
      (with-input-from-string (with-output-to-string (λ () (send-message vtriangle-serialized)))
        map-editor)))
   vtriangle-serialized
   "checking the editor"))


(module+ picts
  (with-input-from-string (with-output-to-string (λ () (send-message vtriangle-serialized)))
    map-editor))
