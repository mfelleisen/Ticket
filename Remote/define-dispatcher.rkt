#lang racket

;; create a dispatcher from types 

(provide
 define-dispatcher)

;; ---------------------------------------------------------------------------------------------------
(require Trains/Common/json)
(require (for-syntax syntax/parse))
(require (for-syntax (only-in racket ~a)))

;; ---------------------------------------------------------------------------------------------------
(begin-for-syntax
  (define-syntax-class dispatcher-clause
    (pattern [(method:id arg:id ...) return:id])))

(define-syntax (define-dispatcher stx)
  (syntax-parse stx
    [[_ dispatcher c:dispatcher-clause ...]
     #:with player #'player
     #:with ((pattern right-hand-side) ...) (map (match-clause #'player) (syntax->list #'[ c ...]))
     #:with (pat1 pat ... patN) #'(pattern ...)
     #:with (rhs1 rhs ... rhsN) #'(right-hand-side ...)
     #`(define ([dispatcher player done?] j)
         (match j
           [(? eof-object?) #false]
           [pat1 (set-box! done? 'go) rhs1]
           [pat  rhs]
           ...
           [patN (set-box! done? #t) rhsN]
           [ill (error 'dispatcher "server sent ill-formed message: ~e" ill)]))]))

(define-for-syntax ((match-clause player) stx)
  (syntax-parse stx
    [[(method:id arg:id ...) return:id]
     #:with (names pat) (create-pattern #'(method arg ...))
     #:with right-hand-side (create-rhs player #'return #'method #'names)
     (list #'pat #'right-hand-side)]))

(define-for-syntax (create-pattern stx)
  (syntax-parse stx
    [(method:id arg:id ...)
     #:with method-str    (symbol->string (syntax-e #'method))
     #:with (x ...)       (generate-temporaries #'(arg ...))
     #:with (arg? ...)    (for/list ([a (syntax->list #'(arg ...))]) (predicate-name a))
     #:with (p-arg ...)   (for/list ([a (syntax->list #'(arg ...))]) (parse-name a))
     #:with (arg-pat ...) #'[(app p-arg (? arg? x)) ...]
     (list #'(x ...) #'`[method-str [,arg-pat ...]])]))

(define-for-syntax (create-rhs player return method names)
  #`(#,(jsexpr-name return) (send #,player #,method #,@names)))

(define-for-syntax (parse-name id)
  (datum->syntax id (string->symbol (~a "parse-" (syntax-e id))) id id))

(define-for-syntax (predicate-name id)
  (datum->syntax id (string->symbol (~a (syntax-e id) "?")) id id))

(define-for-syntax (jsexpr-name id)
  (datum->syntax id (string->symbol (~a (syntax-e id) "->jsexpr")) id id))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define (parse-symbol x) (string->symbol x))
  
  (define player%
    (class object%
      (super-new)
      (define/public (pick x) (displayln `[I picked ,x]))
      (define/public (done x) (displayln `[I am done: ,x]) "I won")))
 
  (define-dispatcher dispatcher 
    [(pick symbol) void]
    [(done boolean) string])

  (define player (new player%))

  ([dispatcher player (box 1)] '["pick" ["a"]])
  ([dispatcher player (box 1)] '["done" [#false]])
  (with-handlers ([exn:fail? (λ (xn) (void))])
    [(dispatcher player (box 0)) '["done" []]]
    (raise 'ouch)))
