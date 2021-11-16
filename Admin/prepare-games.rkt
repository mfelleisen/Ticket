#lang racket

;; allocate a list of items to a list of buckets of max/min size 

#| items ~ players 
The manager starts by assigning them to games with the maximal number of participants permitted.
Once the number of remaining players drops below the minimal number and can't form a game, the
manager backtracks one game and tries games of size one less than the maximal number and so on
until all players are assigned. 

The function preserves the order of the players it is given. 
|#

;                                                                  
;                                                                  
;                                                            ;;;   
;                     ;                                        ;   
;                     ;                                        ;   
;    ;;;;   ;;  ;;  ;;;;;;   ;;;;    ;;;;   ; ;;;     ;;;      ;   
;   ;    ;   ;  ;     ;     ;    ;   ;;  ;  ;;   ;   ;   ;     ;   
;   ;;;;;;    ;;      ;     ;;;;;;   ;      ;    ;       ;     ;   
;   ;         ;;      ;     ;        ;      ;    ;   ;;;;;     ;   
;   ;         ;;      ;     ;        ;      ;    ;  ;    ;     ;   
;   ;;   ;   ;  ;     ;     ;;   ;   ;      ;    ;  ;   ;;     ;   
;    ;;;;;  ;    ;     ;;;   ;;;;;   ;      ;    ;   ;;; ;      ;;;
;                                                                  
;                                                                  
;                                                                  
;                                                                  

(provide
 (contract-out
  [prepare-games 
   (->i ([min-per-game natural?]
         [max-per-game (min-per-game) (and/c natural? (>/c min-per-game))]
         [players (min-per-game) (and/c list? (λ (l) (>= (length l) min-per-game)))])
        (r any/c)
        #:post/name (players r) "same order"
        (equal? (apply append r) players)
        #:post/name (min-per-game max-per-game players r) "proper sizes"
        (andmap (λ (l) (<= min-per-game (length l) max-per-game)) r))]))

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
  (require rackunit))

;                                                          
;                                                          
;                                                          
;                                                          
;                                                          
;   ; ;;;    ;;;;    ;;;;   ; ;;;     ;;;    ;;;;    ;;;;  
;   ;;  ;;   ;;  ;  ;    ;  ;;  ;;   ;   ;   ;;  ;  ;    ; 
;   ;    ;   ;      ;;;;;;  ;    ;       ;   ;      ;;;;;; 
;   ;    ;   ;      ;       ;    ;   ;;;;;   ;      ;      
;   ;    ;   ;      ;       ;    ;  ;    ;   ;      ;      
;   ;;  ;;   ;      ;;   ;  ;;  ;;  ;   ;;   ;      ;;   ; 
;   ; ;;;    ;       ;;;;;  ; ;;;    ;;; ;   ;       ;;;;; 
;   ;                       ;                              
;   ;                       ;                              
;   ;                       ;                              
;                                                          

(define (prepare-games min-per-game  max-per-game lop0)
  (define-values (games remainder) (allocate min-per-game max-per-game lop0))
  (cond
    [(empty? remainder) (reverse games)]
    [(backtrack min-per-game (- max-per-game 1) (append (first games) remainder) (rest games))
     => values]
    [else (error 'prepare-games "the algorithm doesn't work: ~e"
                 `[,min-per-game ,max-per-game ,(length lop0)])]))

#; {N N [Lstof X] [NEListof [NEListof X]] -> [U False [Listof [Listof X]]]}
(define (backtrack min-per-game per-game remainder games)
  (cond
    [(< per-game min-per-game) #false]
    [else
     (define-values (games1 remainder1) (allocate min-per-game per-game remainder))
     (cond
       [(empty? remainder1) (reverse (append games1 games))]
       [(backtrack min-per-game (- per-game 1) remainder games) => values]
       [(cons? games) (backtrack min-per-game per-game (append (first games) remainder) (rest games))]
       [else #false])]))

#; {N N [Listof X] -> (values [Listof [Listof X]] [Listof X])}
;; allocate as many X per-game until the given list is exhausted;
;; if the remainder is still greater than min-per, allocate it; otherwise retyrn it as second value 
(define (allocate min-per-game per-game lop0)
  [define N (length lop0)]
  (let loop ([lop lop0] [games '()] [n N])
    (cond
      [(zero? n) (values games '[])]
      [(< n min-per-game)      (values games lop)]
      [(< n per-game)          (values (cons lop games) '[])]
      [else (loop (drop lop per-game) (cons (take lop per-game) games) (- n per-game))])))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-equal? (prepare-games 2 4 '(a b c)) '[(a b c)])
  (check-equal? (prepare-games 2 4 '(a b c d)) '[(a b c d)])
  (check-equal? (prepare-games 2 4 '(a b c d e)) '[(a b c) (d e)])
  (check-equal? (prepare-games 2 4 '(a b c d e f)) '[(a b c d) (e f)])
  (check-equal? (prepare-games 2 4 '(a b c d e f g)) '[(a b c d) (e f g)])
  (check-equal? (prepare-games 2 4 '(a b c d e f g h)) '[(a b c d) (e f g h)])

  (check-equal? (prepare-games 3 5 '(a b c)) '[(a b c)])
  (check-equal? (prepare-games 3 5 '(a b c d)) '[(a b c d)])
  (check-equal? (prepare-games 3 5 '(a b c d e f)) '[(a b c) (d e f)])
  (check-equal? (prepare-games 3 5 '(a b c d e f g)) '[(a b c d) (e f g)])
  (check-equal? (prepare-games 3 5 '(a b c d e f g h)) '[(a b c d e) (f g h)])
  (check-equal? (prepare-games 3 5 '(a b c d e f g h i j)) '[(a b c d e) (f g h i j)])
  (check-equal? (prepare-games 3 5 '(z y x u v a b c d e f)) '[(z y x u v) (a b c) (d e f)])

  (check-equal? 
   (let ([r (prepare-games 6 8 (build-list 26 (λ (i) (integer->char (+ (char->integer #\a) i)))))])
     (map length r))
   '[8 6 6 6]
   "Ben's test case")

  (check-exn #px"doesn't work" (λ () (prepare-games 7 8 '[a b c d e f g h i]))))