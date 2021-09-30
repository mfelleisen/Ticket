#lang racket

;; de/serialize actions between the server-based referee and remote players 

(provide
 action->jsexpr
 parse-action)

;; -----------------------------------------------------------------------------
(require Trains/Common/player-interface)
(require Trains/Common/connection)

;; -----------------------------------------------------------------------------
(define (action->jsexpr c0)
  (match c0
    [(? (curry equal? MORE)) c0]
    [c (connection-serialize c)]))

(define (parse-action j)
  (match j
    [(? (curry equal? MORE)) j]
    [a (define (check candidate)
         (or candidate
             (displayln `[not a connection, j] (current-error-port))
             #false))
       (parse-connection j #:check check)]))