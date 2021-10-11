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
 #; { [GameMap] [#:edit Boolean] -> GameMap}
 ;; a map visualizer [& editor] that produces a visual representation of the given
 ;; GameMap, which includes width, height, and city placement data. The graph part 
 ;; of the GameMap includes color and segment# data for the city connections. 

 ;; The GameMap is optional. If it is missing, the function reads it from STDIN (piped in file).
 ;; When the #:edit argument is #true, the program enables editing 
 map-editor

 #; Natural
 ;; parameter that controls how long map-editor displays its view 
 max-visualize-time)

(module+ homework
  (provide
   #; {[Listof InternalConnection] [List String [List N N]] Image -> Image}
   draw-connections
   
   #; {[List String [List N N]] Image -> Image}
   draw-cities))

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
(require Trains/Common/connection)
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
  (require (submod Trains/Common/map examples))
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
(define BACKG   (scale .8 (bitmap/file MAP)))
(define CITY    (circle 10 'solid 'red))
(define BCOLOR  'black)
(define FSIZE   22)
(define FCOLOR  'white)
(define CITY?   "Do you want a city here?")
(define CANCEL? "Cancel")
(define NAME    "Enter the city's name")

(require json)

#; {-> GameMap}
;; pop up map editor, wait for user to add cities to map & connections, construct map rep.
(define (map-editor [g #false] #:edit [edit #false])
  (define-values (cities0 connections0 background)
    (match (or g (parse-game-map (read-json)))
      [(? boolean?)    (values '[] '[] BACKG)]
      [(? game-map? g) (game-map-internal g)]))
  (match-define (list cities connections) (edit-graph cities0 connections0 background edit))
  (construct-game-map (image-width background) (image-height background) cities connections))

#; {GameMap -> (values InternalCities InternalConnections Image)}
(define (game-map-internal g)
  (define cities (map (lambda (x) (cons (~a (first x)) (rest x))) (game-map-locations g)))
  (define conns  (external->internal-connections (game-map-all-connections g)))
  (define backg  (rectangle (game-map-width g) (game-map-height g) 'solid BCOLOR))
  (values cities conns backg))

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
#; {type InternalCities     = [Listof [List String [List N N]]]}
;; drawing and presenting choices works with strings, not symbols

(define (external->internal-connections externals)
  (for/list ([c (in-set externals)])
    (list (~a (connection-from c)) (~a (connection-to c)) (connection-color c) (connection-seg# c))))

(define (internal->external-connections internal-connections)
  (for/list ([c internal-connections])
    (define color-seg (cddr c))
    (connection (list-cities (string->symbol (first c)) (string->symbol (second c)))
                (first color-seg)
                (second color-seg))))

(define (internal->external-cities cities)
  (map (λ (x) (cons (string->symbol (first x)) (rest x))) cities))

#; {[Listof InternalCities]
    [Listof InternalConnection]
    Image
    Boolean
    -> [List [Listof InternalCities] [Listof InternalConnection]]}
(define (edit-graph nod0 c0 backg edit)
  (define connection (new connection% [*connections c0] [x0 (+ (image-width backg) 55)] [edit edit]))
  (unless edit (send connection stop))
  (define cities (internal->external-cities (edit-with-big-bang nod0 connection backg edit)))
  (define conns  (internal->external-connections (send connection view)))
  (send connection stop)
  (list cities conns))

(define max-visualize-time (make-parameter 100000000)) ;; for testing
#; {[Listof InternalCities] [Listof InteranlConnections] Image -> [Listof InternalCities]}
(define (edit-with-big-bang nod0 connection backg edit)
  (big-bang nod0
    [to-draw (draw-graph connection backg)]
    [on-mouse (if edit (add-city connection) (λ (s . _) s))]
    ;; this just triggers a regular update of the image w/ new connections 
    [on-tick values 1 (max-visualize-time)]
    [close-on-stop #true]))

#; {[Instance Connection%] -> [Listof InternalCities] N N MouseEvent -> [Listof InternalCities]}
(define ((add-city connection) cities x y me)
  (cond
    [(not (mouse=? "button-down" me)) cities]
    [(eq? 'no (message-box/custom "" "" CITY? CANCEL? #f #f '[number-order default=1])) cities]
    [else
     (define name (get-text-from-user NAME ""))
     (cond
       [(and (city? name) (not (member name (map first cities))))
        (send connection add (for/list ([to cities]) (list name (first to))))
        (cons (list name (list x y)) cities)]
       [else cities])]))

(define connection% ;; manage InternalConnections via edior and for later use 
  (class object% (init-field *connections) (init x0 [edit #true])
    (super-new)

    (define ct (make-custodian))
    (define/public (stop) (custodian-shutdown-all ct))
    
    (define manage
      (parameterize ([current-custodian ct])
        (parameterize ([current-eventspace (make-eventspace)])
          (define frame (new frame% [label "edit connections"] [width 300] [height 300]))
          (manage-connections
           #:frame (and edit frame)
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
  (define +cities (draw-cities cities background))
  (define +conns  (draw-connections (send connection view) cities +cities))
  +conns)

#; {[Listof InternalCities] Image -> Image}
;; add cities at the specified places to the given background image 
(define (draw-cities cities background)
  (for/fold ([image background]) ([n cities])
    (match-define [list name [list x y]] n)
    (define +city (place-image CITY x y image))
    (define +name (place-image (text name FSIZE FCOLOR) x y +city))
    +name))

#; {[Listof InternalConnection] [Listof InternalCities] Image -> Image}
;; add connections at the specified places to the given background image
;; the connections are offset with magic numbers here (hmph)
(define (draw-connections edges cities background)
  (for/fold ([image background]) ([edge-set (group-by (λ (x) (take x 2)) edges)])
    (for/fold ([image image]) ([x edge-set][j (in-naturals)])
      (define i (* 10 j))
      (match-define [list from to c s] x)
      (match-define [list x1 y1] (lookup from cities))
      (match-define [list x2 y2] (lookup to cities))
      (add-segments image s (+ x1 i) (+ y1 i) (+ x2 i) (+ y2 i) c BCOLOR))))

#; {String [Listof InternalCities] -> [List N N]}
(define (lookup name cities)
  (second (first (memf (λ (n) (equal? (first n) name)) cities))))

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

  (define-values (cities conns backg)
    (game-map-internal
    (with-input-from-string (with-output-to-string (λ () (send-message vrectangle-serialized)))
      (λ () (parse-game-map (read-json))))))

  (define background (rectangle 800 800 'solid 'black))
  (define +c (draw-cities cities background))
  (define +s (draw-connections conns cities +c))
  
  (check-equal?
   (parameterize ((max-visualize-time 1))
     (game-map->jsexpr 
      (with-input-from-string (with-output-to-string (λ () (send-message vtriangle-serialized)))
        map-editor)))
   vtriangle-serialized
   "checking the editor"))


(module+ picts
  (map-editor vtriangle)

  (with-input-from-string (with-output-to-string (λ () (send-message vtriangle-serialized)))
    map-editor))
