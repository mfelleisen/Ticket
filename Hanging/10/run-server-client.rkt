#lang racket

(provide
 LOCALHOST 
 
 #; {Port# -> Void}
 ;; runs the server locally on port `p`
 run-server

 #; {Port# [Listof Player] -> Void}
 run-clients 
 
 #; {String [N N] -> PortNumber}
 string->port-number)

;; ---------------------------------------------------------------------------------------------------
(require Trains/Server/server)
(require Trains/Client/client)
(require Trains/Player/buy-now-strategy)
(require Trains/Player/player)
(require SwDev/Testing/make-client)
(require SwDev/Testing/communication)
(require (for-syntax syntax/parse))
(require (for-syntax racket))

;; ---------------------------------------------------------------------------------------------------
(define LOCALHOST "127.0.0.1")


;; ---------------------------------------------------------------------------------------------------
(define (run-server p man-spec)
  (unless (port/c p)
    (error 'xserver "port number expected, given ~e" p))
  (define config
    (let* ([s  DEFAULT-CONFIG]
           [s (hash-set s PORT p)]
           [s (hash-set s QUIET #false)]
           [s (hash-set s MAN-SPEC man-spec)])
      s))
  (server config))

;; ---------------------------------------------------------------------------------------------------
(define (run-clients port players ip)
  (unless (port/c port)
    (error 'xclient "port number expected, given ~e" port))
  
  (client players port #t ip #:quiet #true))

;; ---------------------------------------------------------------------------------------------------
(define (string->port-number p-str [low 10000] [high 60000])
  (define p (string->number p-str))
  (unless (and p (port-number? p) (<= low p high))
    (error 'xclient "port number expected in range [~a, ~a], given ~e" low high p-str))
  p)

;; ---------------------------------------------------------------------------------------------------

(provide
 #; {String -> Boolean}
 ;; does this name specify a different kind of player?
 player-kind?

 #; {String #:gm Gameap #:name String -> [Instanceof Player]}
 ;; make a player of the given kind and map
 make-different-kind-of-player)

(define-syntax (def/kinds stx)
  (syntax-parse stx
    [(_ name player ...)
     #:do [(define lbls (map (λ (x) (~a "player-bad-" (syntax-e x))) (syntax->list #'(player ...))))
           (define clss (map (λ (x) (datum->syntax stx (string->symbol (~a x "%")) stx stx)) lbls))]
     #:with (entry ...) (map (λ (x y) #`(list #,x #,y)) lbls clss)
     #`(define name (list entry ...))]))

(def/kinds KINDS start setup pick play more win end)

(define (player-kind? x)
  (assoc x KINDS))

(define (make-different-kind-of-player kind #:gm gm #:name name)
  (define player% (second (assoc kind KINDS)))
  (new player% [strategy% strategy%] [the-map gm] [name name]))