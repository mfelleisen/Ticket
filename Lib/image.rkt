#lang racket

(define color? (or/c string? symbol?))

(provide
 (contract-out
  [add-segments
   #; (add-segments img n x1 y1 x2 y2 Lcolor Bcolor)
   ;; adds `n` Lcolored line segments between (x1,y1) and (x2,y2)
   ;; if the background color is Bcolor 
   (-> image? natural? natural? natural? natural? natural? color? color? image?)]))

;; -----------------------------------------------------------------------------
(require 2htdp/image)

;; -----------------------------------------------------------------------------
(define (add-segments background n x1 y1 x2 y2 line-color background-color)
  (define s (add-line background x1 y1 x2 y2 line-color))
  (define delta-x (quotient (- x2 x1) n))
  (define delta-y (quotient (- y2 y1) n))
  (for/fold ([s s]) ([i (in-range 1 n 1)])
    (define x-mid (+ x1 (* i delta-x)))
    (define y-mid (+ y1 (* i delta-y)))
    (place-image (circle 3 'solid background-color) x-mid y-mid s)))

;; -----------------------------------------------------------------------------
;; this isn't really a test 
(module+ picts
  (define WIDTH 200)
  (define HEIGHT 300)

  (define A (list (random WIDTH) (random HEIGHT)))
  (define B (list (random WIDTH) (random HEIGHT)))

  (define BACK
    (let* ([s (rectangle WIDTH HEIGHT 'solid 'white)]
           [s (place-image (text "A" 11 'black) (first A) (second A) s)]
           [s (place-image (circle 5 'solid 'red) (first A) (second A) s)]
           [s (place-image (text "B" 11 'black) (first B) (second B) s)]
           [s (place-image (circle 5 'solid 'red) (first B) (second B) s)])
      s))
  (add-segments BACK 5 (first A) (second A) (first B) (second B) 'blue 'white))