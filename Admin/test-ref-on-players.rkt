#lang racket

;; a stress test 

;                                                                  
;                                                                  
;                                                                  
;                                                     ;            
;                                                     ;            
;    ;;; ;   ;;;;   ; ;;;    ;;;;    ;;;;;    ;;;   ;;;;;;   ;;;;  
;    ;  ;;   ;  ;;  ;;   ;   ;  ;;   ;;      ;   ;    ;      ;  ;; 
;   ;    ;  ;    ;  ;    ;  ;    ;   ;           ;    ;     ;    ; 
;   ;    ;  ;;;;;;  ;    ;  ;;;;;;   ;       ;;;;;    ;     ;;;;;; 
;   ;    ;  ;       ;    ;  ;        ;      ;    ;    ;     ;      
;   ;    ;  ;       ;    ;  ;        ;      ;    ;    ;     ;      
;    ;  ;;   ;      ;    ;   ;       ;      ;   ;;    ;      ;     
;    ;;; ;   ;;;;;  ;    ;   ;;;;;   ;       ;;; ;     ;;;   ;;;;; 
;        ;                                                         
;    ;  ;;                                                         
;     ;;;                                                          
;                                                                  

(module generate racket
  (provide tests)

  (require Trains/Common/map-serialize)
  (require Trains/Common/map)
  (require json)

  (define (the-map file-name)
    (let* ([f (file-exists? file-name)]
           [f (and f (with-input-from-file file-name read-and-parse-map))]
           [s (or f (construct-random-map 200 800 (build-list 20 (compose string->symbol ~a)) 40))]
           [j (if f #false (game-map->jsexpr s))]
           [_ (and j (with-output-to-file file-name (λ () (write-json j)) #:exists 'replace))])
      s))

  (define tests
    (for/list ([i 5])
      (the-map (~a "map-" i ".json")))))

;                                                                                          
;                                                                                          
;                                                                                          
;             ;                                               ;                       ;    
;             ;                                               ;                       ;    
;    ;;;;   ;;;;;;   ;;;;;   ;;;;    ;;;;    ;;;;           ;;;;;;   ;;;;    ;;;;   ;;;;;; 
;   ;    ;    ;      ;;      ;  ;;  ;    ;  ;    ;            ;      ;  ;;  ;    ;    ;    
;   ;         ;      ;      ;    ;  ;       ;                 ;     ;    ;  ;         ;    
;   ;;;       ;      ;      ;;;;;;  ;;;     ;;;               ;     ;;;;;;  ;;;       ;    
;      ;;;    ;      ;      ;          ;;;     ;;;            ;     ;          ;;;    ;    
;        ;    ;      ;      ;            ;       ;            ;     ;            ;    ;    
;   ;    ;    ;      ;       ;      ;    ;  ;    ;            ;      ;      ;    ;    ;    
;    ;;;;      ;;;   ;       ;;;;;   ;;;;    ;;;;              ;;;   ;;;;;   ;;;;      ;;; 
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          

(module test racket

  (require (submod ".." generate))
  (require Trains/Admin/referee)
  (require Trains/Player/player)
  (require Trains/Player/hold-10-strategy)
  (require Trains/Common/map)
  (require Trains/Admin/cards)

  #; {Ranking -> Void}
  (define (display-results results)
    (for ([rank (first results)] [r (in-naturals)])
      (for ([p rank])
        (displayln `[,(get-field name p) placed ,(show-rank (+ r 1))]))))

  #;{N -> String}
  (define (show-rank i)
    (case i
      [(1) "first"]
      [(2) "second"]
      [(3) "third"]
      [else (~a i "th")]))
  (define the-map 10)
  (define the-file 10)

  (define (make-players i)
    (build-list i (λ (i) (make-player #:strategy hold-10-strategy% #:name (~a "player" i)))))

  (define (run-test the-map)
    (define results (referee (make-players 8) the-map #:cards cards-handed-out))
    (display-results results))

  (for ([test-case tests]) (run-test test-case)))

#;
(module+ picts
  (require (submod Trains/Editor/map-editor homework))
  (require (prefix-in im: 2htdp/image))
  
  #; {[List String [List N N]] Image -> Image}
  (define (external->internal-connections externals)
    (for/list ([c (in-set externals)])
      (list* (~a (connection-from c)) (~a (connection-to c)) (cddr c))))

  (define cities (map (lambda (x) (cons (~a (first x)) (rest x))) (game-map-locations the-map)))
  (define conns  (external->internal-connections (game-map-all-connections the-map))) 
  (define +cities (draw-cities cities (im:empty-scene 200 800)))
  (draw-connections conns cities +cities))
