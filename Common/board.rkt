#lang racket

;; a bi-directional graph representation of the railroad map

(provide
 #; {type Path = [Listof Connection]}
 #; {Graph City City [Listof Connection]  -> [Listof Path]}
 #; (all-paths A B blocked)
 ;; produces a list of all open paths from `A` to `B`
 all-paths

 to-city to-color to-seg#)

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
  (provide triangle bad))

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

;; A data structure that enforces the logical invariant can be used for JSON
;; but it's not what we want for planning paths---which is a frequent operation. 

(struct to [city color seg#] #:transparent)

#; {type Graph = [Hashof String [Listof Connection]]}
;; maps city to all existing connections
#; {type Connection = (to City Color Seg#)}
#; {type City = Symbol}
#; {type Color = Symbol}
#; {type Seg# = (U 3 4 5)}
;; -- target city, its connection color, and the number of segments

;; ASSMMTIONS:
;; -- cities are consistently named (keep separate city to posn map for drawing)
;; -- connection are two-ways (undirected)

(module+ examples
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

#; {Graph City -> [Listof Connection]}
(define (lookup graph city blocked)
  (define connections (hash-ref graph city #false))
  (when (boolean? connections)
    (displayln `[graph domain: ,(map car (hash->list graph))] (current-error-port))
    (error 'graph-lookup "can't happend, city not found ~e" city))
  (remove* blocked connections))

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