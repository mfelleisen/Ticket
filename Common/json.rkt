#lang racket

;; for generating serializing and deserializing plain values from and to JSexpr

(provide
  parse-boolean boolean->jsexpr
  parse-natural natural->jsexpr
  parse-void    void->jsexpr
  parse-string  string->jsexpr)

;; ---------------------------------------------------------------------------------------------------

; natural? 
(define (parse-natural x) (and (natural? x) x))
(define (natural->jsexpr n) n)

; boolean?
(define (parse-boolean x) x)
(define (boolean->jsexpr b) b)

; void?
(define (parse-void j) (match j ["void" (void)]))
(define (void->jsexpr v) "void")

; string?
(define parse-string values)
(define string->jsexpr values)
