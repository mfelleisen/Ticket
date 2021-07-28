#lang racket

(provide
 width?
 height?
 city?

 #; {Symbol Symbol -> [List Symbol Symbol] :: ordered via symbol<?}
 list-cities
 
 color?
 seg#?

 COLORS
 SEG#
 MIN-WIDTH  MAX-WIDTH
 MIN-HEIGHT MAX-HEIGHT

 ;; for homework only 
 CITY-LENGTH
 CITY-NAME)

;; -----------------------------------------------------------------------------
(define COLORS '[red blue green white])

(define (color? x) (member (if (string? x) (string->symbol x) x) COLORS))

(define SEG# '[3 4 5])

(define (seg#? x) (member x SEG#))

(define CITY-NAME "[a-zA-Z0-9\\ \\.\\,]+")
(define CITY-LENGTH 25)
(define pxCITY (pregexp CITY-NAME))

(define (city? s)
  (and (or (string? s) (symbol? s))
       (let ([s (if (string? s) s (~a s))])
         (<= 1 (string-length s) CITY-LENGTH)
         (let ([m (regexp-match pxCITY s)])
           (and (cons? m) (equal? (first m) s))))))

(define (list-cities start end)
  (if (symbol<? start end) (list start end) (list end start)))

(define MIN-WIDTH 10)
(define MAX-WIDTH 800)
(define MIN-HEIGHT 10)
(define MAX-HEIGHT 800)

(define (width? x)
  (and (natural? x) (<= MIN-WIDTH x MAX-WIDTH)))

(define (height? x)
  (and (natural? x) (<= MIN-HEIGHT x MAX-HEIGHT)))

;; -----------------------------------------------------------------------------
(module+ test
  (require rackunit)

  (define DC  "Washington, D.C.")
  (check-equal? (regexp-match pxCITY DC) `[,DC])
  (check-true (city? DC)))
