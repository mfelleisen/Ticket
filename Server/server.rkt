#lang racket

;; a tournament server that signs up players over TCP and runs a tournament 

;                                                          
;                                                          
;                                                          
;                                             ;            
;                                             ;            
;    ;;;;   ;   ;;  ; ;;;    ;;;;    ;;;;;  ;;;;;;   ;;;;  
;    ;  ;;   ;  ;   ;;  ;   ;;  ;;   ;;       ;     ;    ; 
;   ;    ;    ;;    ;    ;  ;    ;   ;        ;     ;      
;   ;;;;;;    ;;    ;    ;  ;    ;   ;        ;     ;;;    
;   ;         ;;    ;    ;  ;    ;   ;        ;        ;;; 
;   ;         ;;    ;    ;  ;    ;   ;        ;          ; 
;    ;       ;  ;   ;;  ;   ;;  ;;   ;        ;     ;    ; 
;    ;;;;;  ;    ;  ; ;;;    ;;;;    ;         ;;;   ;;;;  
;                   ;                                      
;                   ;                                      
;                   ;                                      
;                                                          

(require (only-in Trains/Admin/manager results/c))
(require Fish/Lib/hash-contract)

(define port/c (and/c natural-number/c (</c 60000) (>/c 10000)))
(define player#/c natural-number/c)

(define PORT 'port)
(define SERVER-TRIES  'server-tries)
(define SERVER-WAIT   'server-wait)
(define T-PLAYERS     't-players)
(define TIME-PER-TURN 'time-per-turn)
(define MAN-SPEC      'manager-specific)

(define options (list PORT SERVER-TRIES SERVER-WAIT TIME-PER-TURN))

(provide
 ;; server options 
 PORT SERVER-TRIES SERVER-WAIT T-PLAYERS TIME-PER-TURN 

 (contract-out
  [server
   #; (server player#/c wait-for-sec port#)
   ;; returns the list of winners and cheaters/failures 
   ;; runsning an manager on the players that connected on port# in time
   ;; plus the house players (if any) 
   ;; wait-for-sec seconds or N >= player# as soon as that many signed up 
   (->i ([config (hash-carrier/c options)]) ([players any/c]) (r results/c))]))

;                                                                                      
;       ;                                  ;                                           
;       ;                                  ;                          ;                
;       ;                                  ;                                           
;    ;;;;   ;;;   ;;;;    ;;;   ; ;;    ;;;;   ;;;   ; ;;    ;;;    ;;;    ;;;    ;;;  
;   ;; ;;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;;  ;     ;   ;;  ;  ;   ; 
;   ;   ;  ;   ;; ;   ;  ;   ;; ;   ;  ;   ;  ;   ;; ;   ;  ;         ;   ;   ;; ;     
;   ;   ;  ;;;;;; ;   ;  ;;;;;; ;   ;  ;   ;  ;;;;;; ;   ;  ;         ;   ;;;;;;  ;;;  
;   ;   ;  ;      ;   ;  ;      ;   ;  ;   ;  ;      ;   ;  ;         ;   ;          ; 
;   ;; ;;  ;      ;; ;;  ;      ;   ;  ;; ;;  ;      ;   ;  ;;        ;   ;      ;   ; 
;    ;;;;   ;;;;  ;;;;    ;;;;  ;   ;   ;;;;   ;;;;  ;   ;   ;;;;   ;;;;;  ;;;;   ;;;  
;                 ;                                                                    
;                 ;                                                                    
;                 ;                                                                    

(require Trains/Remote/player)
(require Trains/Lib/xsend)
(require (except-in Trains/Admin/manager results/c))

(require SwDev/Testing/communication)

(module+ test
  (require (submod ".."))
  (require Trains/Common/basic-constants)
  (require Trains/Client/client)
  (require Trains/Player/player)
  (require Trains/Player/hold-10-strategy)
  (require (submod Trains/Admin/referee examples))
  (require (submod Trains/Admin/manager examples))
  (require rackunit))

;                                            
;                                            
;                                            
;                                            
;    ;;;    ;;;    ;;;;  ;   ;   ;;;    ;;;; 
;   ;   ;  ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ;
;   ;      ;   ;;  ;      ; ;   ;   ;;  ;    
;    ;;;   ;;;;;;  ;      ; ;   ;;;;;;  ;    
;       ;  ;       ;      ; ;   ;       ;    
;   ;   ;  ;       ;       ;    ;       ;    
;    ;;;    ;;;;   ;       ;     ;;;;   ;    
;                                            
;                                            
;                                            

(define LOCAL     "127.0.0.1")
(define MAX-TCP   30)
(define REOPEN    #t)
(define DEFAULT-RESULT '[[] []])

(define test-run?  (make-parameter #false))
(define MIN-ERROR  "server did not sign up enough ~a players")

;; get at least min players, at most max players
;; wait for at most 30s
;; if during this interval, min players showed up: work with the players you have
;; otherwise, re-start the thread

;; threads plus channels: 
;; create thread and run for time/out
;; if timed out, tell thread "time's up"
;; -- if there are min players, return those and shut down
;; -- otherwise return false, requesting an extension

(define (server config [age-ordering reverse] [house-players '()])
  (define port (dict-ref config PORT))
  (define MAX-TIME    (dict-ref config SERVER-WAIT))
  (define MIN-PLAYERS (dict-ref config T-PLAYERS))
  (define MAX-PLAYERS (dict-ref config T-PLAYERS)) ;; BUG: need to accommodate max and min 
  (define MAX-TRIES   (dict-ref config SERVER-TRIES))

  ;; set up custodian so `server` can clean up all threads, TCP ports in case it is re-used
  (parameterize ([current-custodian (make-custodian)])
    (define players (wait-for-players port house-players MAX-TRIES MAX-TIME MIN-PLAYERS MAX-PLAYERS))
    (begin0 
      (cond
        [(empty? players) (fprintf (current-error-port) MIN-ERROR MIN-PLAYERS) DEFAULT-RESULT]
        [(test-run?) => (λ (result) (channel-put result (age-ordering players)) DEFAULT-RESULT)]
        [else (configure-manager (age-ordering players) config)])
      (custodian-shutdown-all (current-custodian)))))

#; {[Listof Player] ImmutableHash -> [List [Listof Player] [Listof Player]]}
(define (configure-manager players config)
  (define game-time-out (dict-ref config TIME-PER-TURN))
  (define man-specifics (dict-ref config MAN-SPEC '[]))
  (keyword-apply/dict manager man-specifics (list players)))

#;{Port# [Listof Player] Int Int Int -> [Listof Player]}
(define (wait-for-players port house-players MAX-TRIES MAX-TIME MIN-PLAYERS MAX-PLAYERS)
  (define communicate-with-sign-up (make-channel))
  (thread (sign-up-players port communicate-with-sign-up house-players MIN-PLAYERS MAX-PLAYERS))
  (let loop ([n MAX-TRIES])
    (cond
      [(zero? n) '()]
      [(sync/timeout MAX-TIME communicate-with-sign-up) => values] ;; order by age 
      [else
       (channel-put communicate-with-sign-up (~a "are there at least " MIN-PLAYERS " signed up"))
       (cond
         [(channel-get communicate-with-sign-up) => reverse]
         [else (loop (- n 1))])])))

#; {Port Channel [Listof Player] Int Int -> Void}
(define [(sign-up-players port send-players house-players MIN-PLAYERS MAX-PLAYERS)]
  (define listener (tcp-listen port MAX-TCP REOPEN))
  (let collect-players ([players house-players])
    (cond
      [(= (length players) MAX-PLAYERS)
       (channel-put send-players players)]
      [else
       (sync
        (handle-evt listener
                    (λ (_)
                      (collect-players (add-player players listener))))
        (handle-evt send-players
                    (λ (_)
                      (cond
                        [(>= (length players) MIN-PLAYERS) (channel-put send-players players)]
                        [else (channel-put send-players #false) (collect-players players)]))))])))

#; (TCP-Listener [Listof Player] -> [Listof Player])
(define (add-player players listener)
  (with-handlers ((exn:fail:network? (lambda (x) (log-error "connect: ~a" (exn-message x)) players)))
    (define-values (in out) (tcp-accept listener))
    (define name (read-message in))
    (cond
      [(short-string? name)
       (define next (if (test-run?) (add1 (length players)) (make-remote-player in out)))
       (cons next players)]
      [else
       (define m [if (and (string? name) (regexp-match #px"Timed" name)) "name" "not a short string"])
       (displayln (~a "failed to send " m) (current-error-port))
       (close-input-port in)
       (close-output-port out)
       players])))

#; {Any -> Boolean}
(define (short-string? p)
  (and (string? p)
       (regexp-match #px"^[a-zA-Z]*$" p) 
       (<= 1 (string-length p) 12)))

;                                                                                      
;                                                                                      
;     ;       ;             ;                          ;                    ;          
;     ;                                                ;                    ;          
;   ;;;;;   ;;;  ;;;;;;   ;;;   ; ;;    ;;;;         ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;       ;  ;  ;  ;    ;   ;;  ;  ;;  ;           ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;   ;; ;        ;    ;     
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;;;;;;  ;;;     ;     ;;;  
;     ;       ;  ;  ;  ;    ;   ;   ;  ;   ;           ;    ;          ;    ;        ; 
;     ;       ;  ;  ;  ;    ;   ;   ;  ;; ;;           ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;;;  ;  ;  ;;;;; ;   ;   ;;;;           ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                          ;                                           
;                                       ;  ;                                           
;                                        ;;                                            

(module+ test ;; timing

  (define config
    (hash
     PORT       45670
     SERVER-WAIT   20
     T-PLAYERS      5
     SERVER-TRIES   1
     TIME-PER-TURN 10))


  #; { N Port-Number (U False n:N) -> (U False [Listof 0]: (=/c (length) n))}
  #; (run-server-test m p k)
  ;; runs the server on port p, waitig for m players, but receiving k
  (define (run-server-test port k)
    [define custodian (make-custodian)]
    [define result    (make-channel)]
    [define err-out   (open-output-string)]
    (parameterize ([test-run?          result]
                   [current-custodian  custodian]
                   [current-error-port err-out])
      (define config2
        (let* ([config config]
               [config (hash-set config PORT port)]
               [config (if k (hash-set config T-PLAYERS k) config)])
          config))
      (define th (thread (λ () (server config2))))
      (sleep 1)
      (if (boolean? k)
          (sync th)
          (for ([i k])
            (define-values (- +) (tcp-connect LOCAL port))
            (send-message "a" +))))
    (begin0
      (if k (channel-get result) (get-output-string err-out))
      (custodian-shutdown-all custodian)))

  (check-equal? (run-server-test 45671 #f) (format MIN-ERROR 5) "no sign ups")
  (check-equal? (run-server-test 45677 10) (build-list 10 add1) "sign up enough players")
  (check-equal? (run-server-test 45676 9) (build-list  9 add1) "sign up too few players"))

;                                                                 
;      ;;                                                         
;     ;           ;;;    ;;;             ;                    ;   
;     ;             ;      ;             ;                    ;   
;   ;;;;;  ;   ;    ;      ;           ;;;;;   ;;;    ;;;   ;;;;; 
;     ;    ;   ;    ;      ;             ;    ;;  ;  ;   ;    ;   
;     ;    ;   ;    ;      ;             ;    ;   ;; ;        ;   
;     ;    ;   ;    ;      ;             ;    ;;;;;;  ;;;     ;   
;     ;    ;   ;    ;      ;             ;    ;          ;    ;   
;     ;    ;   ;    ;      ;             ;    ;      ;   ;    ;   
;     ;     ;;;;     ;;     ;;           ;;;   ;;;;   ;;;     ;;; 
;                                                                 
;                                                                 
;                                                                 


(module+ test

  (define QUIET #false)
 
  (define (test-server-client-with players bad# (man-spec '[]) (age-ordering reverse))
    (define PORT# 45674)
    (parameterize ([current-custodian (make-custodian)]
                   [time-out-limit 1.2])
      (define err-out  (if QUIET (open-output-string) (current-error-port)))
      (define config3
        (let* ([config config]
               [config (hash-set config PORT PORT#)]
               [config (hash-set config MAN-SPEC man-spec)]
               ;; badly named players drop out: 
               [config (hash-set config T-PLAYERS (- (length players) bad#))]) 
          config))
      (define customer (thread (λ () (parameterize ([current-error-port err-out]) (client players PORT#)))))
      (define result
        (parameterize ([current-error-port err-out])
          (server config3 age-ordering)))
      (begin0
        result
        (sync customer)
        (custodian-shutdown-all (current-custodian)))))
      
  (define PLRS '["a" "b" "c" "d" "e1" "failed attempt at Name" "e"])
  (define players-1
    (map (λ (n) (new player% [name n] [the-map big-map] [strategy% hold-10-strategy%])) PLRS))
  [define result-1 (test-server-client-with players-1 2)]
  (test-case "player 1"
             (check-true (cons? result-1))
             (check-true (empty? (rest (first result-1))) "one winner")
             (check-true (empty? (second result-1)) "no cheaters"))

  (define hold-10# 17)
  (define buy-now#  1)
  [define cheat#   10]
  [define bundles   (make-players big-map hold-10# buy-now# cheat#)]
  [define players-2 (apply append bundles)]
  (define man-spec  `[[#:shuffle . ,sorted-destinations] [#:cards . ,(make-list CARDS-PER-GAME 'white)]])
  [define result-2  (test-server-client-with players-2 0 man-spec reverse)]
  (test-case "player 2"
             (check-true (cons? result-2))
             (check-true (empty? (rest (first result-2))) "one winner")
             (check-equal? (length (second result-2)) cheat# "no cheaters")

             (for-each (λ (p) (displayln (get-field name p))) players-2)
             (displayln 'winners)
             (for-each (λ (p) (displayln (get-field name p))) (second bundles))
             (displayln 'cheaters)
             (for-each (λ (p) (displayln (get-field name p))) (third bundles))
             (displayln 'actual-winners)
             (for-each (λ (p) (displayln (get-field name p))) (first result-2))
             (displayln 'actual-cheaters)  
             (for-each (λ (p) (displayln (get-field name p))) (second result-2))

             (check-equal? (manager-results->names result-2)
                           (manager-results->names `{[,@(second bundles)] ,(third bundles)}))))
