#lang racket

;; compute a sequence of colored cards to be handed out over the course of a game 

(provide
 #; [Listof [List N Color]]
 ;; how may cards are to be handed out 
 cards-handed-out)

(require Trains/Common/basic-constants)

(define kinds   (length COLORS))
(define cards#  200) ;; rails * player#
(define cards   (for/fold ([h (make-hash)]) ({c COLORS}) (hash-set! h c cards#) h))

(define (get-cards c n)
  (define available (hash-ref cards c))
  (cond
    [(< available n)
     (hash-set! cards c 0)
     available]
    [else
     (hash-update! cards c (λ _ (- available n)))
     n]))

(define (! n) (if (zero? n) 1 (* n (! (- n 1)))))

(define (c n)
  (define fact (! n))
  (+ 1 (inexact->exact (round (abs (* (sin (* 10 n)) n))))))

(define (sum l #:show (show #false))
  [define accu 0]
  (for/list ([x l] [i (in-naturals)])
    (set! accu (+ x accu))
    (define next (list-ref COLORS (modulo i (length COLORS))))
    (if show 
        (list (get-cards next x) (~a next) accu)
        (list (get-cards next x) (~a next)))))

(define cards-by-count (sum (build-list 32 c)))

(define (unfold cards-by-count)
  (for/fold ([cards '()]) ([c cards-by-count])
    (append cards (make-list (first c) (string->symbol (second c))))))

(define cards-handed-out (unfold cards-by-count))
