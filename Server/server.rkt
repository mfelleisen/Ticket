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
(require SwDev/Lib/hash-contract)

(define port/c (and/c natural-number/c (</c 60000) (>/c 10000)))
(define player#/c natural-number/c)

(define PORT 'port)
(define SERVER-TRIES  'server-tries)
(define SERVER-WAIT   'server-wait)
(define MAX-T-PLAYERS 'max-t-players)
(define MIN-T-PLAYERS 'min-t-players)
(define TIME-PER-TURN 'time-per-turn)
(define MAN-SPEC      'manager-specific)
(define QUIET         'quiet)

(define options (list PORT SERVER-TRIES SERVER-WAIT TIME-PER-TURN MAN-SPEC QUIET))

(provide
 ;; server options 
 PORT SERVER-TRIES SERVER-WAIT MAX-T-PLAYERS MIN-T-PLAYERS TIME-PER-TURN MAN-SPEC QUIET

 SHORT

 (contract-out
  (DEFAULT-CONFIG (hash-carrier/c options))
  [server
   ;; returns the list of winners and cheaters/failures 
   ;; runsning an manager on the players that connected on port# in time
   ;; plus the house players (if any) 
   ;; wait-for-sec seconds or N >= player# as soon as that many signed up 
   (->i ([confg (hash-carrier/c options)]) ([plyrs any/c] #:result (s (-> list? any/c))) (r any/c))]))

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

(require Trains/Common/basic-constants)
(require Trains/Remote/player)
(require Trains/Lib/xsend)
(require (except-in Trains/Admin/manager results/c))
(require (submod Trains/Admin/manager examples))

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
;                              ;;;     ;                   
;                             ;        ;                   
;                             ;                            
;     ;;;    ;;;;   ; ;;;   ;;;;;;   ;;;     ;;; ;   ;;;;  
;    ;   ;  ;;  ;;  ;;   ;    ;        ;     ;  ;;  ;    ; 
;   ;       ;    ;  ;    ;    ;        ;    ;    ;  ;      
;   ;       ;    ;  ;    ;    ;        ;    ;    ;  ;;;    
;   ;       ;    ;  ;    ;    ;        ;    ;    ;     ;;; 
;   ;       ;    ;  ;    ;    ;        ;    ;    ;       ; 
;    ;   ;  ;;  ;;  ;    ;    ;        ;     ;  ;;  ;    ; 
;     ;;;    ;;;;   ;    ;    ;     ;;;;;;;  ;;; ;   ;;;;  
;                                                ;         
;                                            ;  ;;         
;                                             ;;;          
;                                                          

(define DEFAULT-CONFIG
  (hash
   PORT       45670
   SERVER-WAIT   20
   MIN-T-PLAYERS  5
   MAX-T-PLAYERS 50
   SERVER-TRIES     2
   TIME-PER-TURN  1.8 ;; this is needed on occasion (1.3 might do)
   ;; a lit of optional keyword arguments:
   #; {[#:shuffle . shuffle-proc] [#:cards DOT list-of color]}
   MAN-SPEC     '[]
   QUIET         #true))

(define SHORT MAX-PLAYER-NAME)

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
(define MIN-ERROR  "server did not sign up enough [~a] players")

;; get at least min players, at most max players
;; wait for at most 30s
;; if during this interval, min players showed up: work with the players you have
;; otherwise, re-start the thread

;; threads plus channels: 
;; create thread and run for time/out
;; if timed out, tell thread "time's up"
;; -- if there are min players, return those and shut down
;; -- otherwise return false, requesting an extension

;; `man-spec` is a keyword-dicionary that is turned into arguments to the manager
;; that way the server is parametric wrt to the manager's domain 

(define (server config [age-ordering reverse] [house-players '()] #:result (show void))
  (define port (dict-ref config PORT))
  (define MAX-TIME    (dict-ref config SERVER-WAIT))
  (define MIN-PLAYERS (dict-ref config MIN-T-PLAYERS))
  (define MAX-PLAYERS (dict-ref config MAX-T-PLAYERS))
  (define MAX-TRIES   (dict-ref config SERVER-TRIES))

  ;; set up custodian so `server` can clean up all threads, TCP ports in case it is re-used
  (define err-out  (if (dict-ref config QUIET) (open-output-string) (current-error-port)))
  (parameterize ([current-custodian  (make-custodian)]
                 [current-error-port err-out])
    (define players (wait-for-players port house-players MAX-TRIES MAX-TIME MIN-PLAYERS MAX-PLAYERS))
    (begin0 
      (cond
        [(empty? players) (fprintf (current-error-port) MIN-ERROR MIN-PLAYERS) (show DEFAULT-RESULT)]
        [(test-run?) => (λ (result) (channel-put result (age-ordering players))(show DEFAULT-RESULT))]
        [else (configure-and-run-manager (age-ordering players) config show)])
      (custodian-shutdown-all (current-custodian)))))

#; {[Listof Player] ImmutableHash -> [List [Listof Player] [Listof Player]]}
(define (configure-and-run-manager players config show)
  (define game-time-out (dict-ref config TIME-PER-TURN))
  (define man-specifics (dict-ref config MAN-SPEC '[]))
  (parameterize ([time-out-limit game-time-out])
    (define result (keyword-apply/dict manager man-specifics (list players)))
    (send-message (manager-results->names result))
    (close-output-port (current-output-port))
    (show result)))

#;{Port# [Listof Player] Int Int Int -> [Listof Player]}
(define (wait-for-players port house-players MAX-TRIES MAX-TIME MIN-PLAYERS MAX-PLAYERS)
  (define communicate-with-sign-up (make-channel))
  (thread (sign-up-players port communicate-with-sign-up house-players MIN-PLAYERS MAX-PLAYERS))
  (let loop ([n MAX-TRIES] [min-players MIN-PLAYERS])
    (cond
      [(zero? n) '()]
      [(sync/timeout MAX-TIME communicate-with-sign-up) => values] ;; order by age 
      [else
       (channel-put communicate-with-sign-up min-players)
       (cond
         [(channel-get communicate-with-sign-up) => values]
         [else (loop (- n 1) MIN-PLAYER-PER-GAME)])])))

#; {Port Channel [Listof Player] Int Int -> Void}
(define [(sign-up-players port communicate-w/wait house-players MIN-PLAYERS MAX-PLAYERS)]
  (define listener (tcp-listen port MAX-TCP REOPEN))
  (let collect-players ([players house-players])
    (cond
      [(= (length players) MAX-PLAYERS)
       (channel-put communicate-w/wait players)]
      [else
       (sync
        (handle-evt listener
                    (λ (_)
                      (collect-players (add-player players listener))))
        (handle-evt communicate-w/wait
                    (λ (min-players)
                      (cond
                        [(>= (length players) min-players) (channel-put communicate-w/wait players)]
                        [else (channel-put communicate-w/wait #f) (collect-players players)]))))])))

#; (TCP-Listener [Listof Player] -> [Listof Player])
(define (add-player players listener)
  (with-handlers ((exn:fail:network? (lambda (x) (log-error "connect: ~a" (exn-message x)) players)))
    (define-values (in out) (tcp-accept listener))
    (define name (read-message in))
    (cond
      [(short-string? name)
       (define next (if (test-run?) (add1 (length players)) (make-remote-player name in out)))
       (displayln `[,name sogned up] (current-error-port))
       (cons next players)]
      [else
       (define m
         [if (string? name)
             (if (regexp-match #px"Timed" name)
                 "timed out"
                 name)
             (~a "not a string: " name)])
       (displayln (~a "improper sign-up: " m) (current-error-port))
       (close-input-port in)
       (close-output-port out)
       players])))

#; {Any -> Boolean}
(define (short-string? p)
  (and (string? p)
       (regexp-match #px"^[a-zA-Z]*$" p) 
       (<= 1 (string-length p) SHORT)))

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
        (let* ([config DEFAULT-CONFIG]
               [config (hash-set config PORT port)]
               [config (if k (hash-set config MIN-T-PLAYERS k) config)])
          config))
      (define th (thread (λ () (server config2 #:result values))))
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
  (define (test-server-client-with players bad-during-signup#
                                   (man-spec '[])
                                   (age-ordering reverse)
                                   (config0 DEFAULT-CONFIG))
    (define PORT# 45674)
    (parameterize ([current-custodian (make-custodian)])
      (define config3
        (let* ([config config0]
               [config (hash-set config PORT PORT#)]
               [config (hash-set config MAN-SPEC man-spec)]
               ;; badly named players drop out: 
               [config (hash-set config MAX-T-PLAYERS (- (length players) bad-during-signup#))]) 
          config))
      (define err-out  (if (dict-ref config3 QUIET) (open-output-string) (current-error-port)))
      (define customer
        (thread
         (λ ()
           (parameterize ([current-error-port err-out])
             (client players PORT#)))))
      (define result
        (parameterize ([current-error-port err-out])
          (server config3 age-ordering #:result values)))
      (begin0
        result
        (sync customer)
        (custodian-shutdown-all (current-custodian)))))

  ;; - - - test case 1 
  (define PLRS '["a" "b" "c" "d" "e1" "failed attempt at Name" "e"])
  (define players-1
    (map (λ (n) (new player% [name n] [the-map big-map] [strategy% hold-10-strategy%])) PLRS))
  [define result-1 (test-server-client-with players-1 2)]
  (test-case "player 1"
             (check-true (cons? result-1))
             (check-true (empty? (rest (first result-1))) "one winner")
             (check-true (empty? (second result-1)) "no cheaters"))) 

(module+ test ;; real players 

  #; {GameMap N N N -> Test}
  ;; the numbers cannot be chosen freely
  ;; assumes that hold-10s are stupid, all buy-nows win 
  (define (check-manager the-map hold-10# buy-now# cheat# [baddy% #false] [bad# 0])
    (match-define [list hold-10s buy-nows cheaters] (make-players the-map hold-10# buy-now# cheat#))
    (define man-spec    `[[#:shuffle . ,sorted-destinations]
                          [#:cards . ,(make-list CARDS-PER-GAME 'white)]])
    (define qconfig     (let* ([s DEFAULT-CONFIG]
                               [s (hash-set s QUIET #true)]
                               [s (hash-set s MIN-T-PLAYERS (hash-ref s MIN-T-PLAYERS))])
                          s))
    (define bad-players (make-baddies the-map baddy% bad#))
    (define all-players (append hold-10s buy-nows cheaters bad-players))
    (define the-results (test-server-client-with all-players 0 man-spec reverse qconfig))
    (check-equal? (manager-results->names the-results)
                  (manager-results->names `{[,@buy-nows] ,(append bad-players cheaters)})
                  (~a hold-10# buy-now# cheat# baddy% bad#)))
  
  (check-manager big-map 17 1 10)
  (check-manager big-map 27 1 12)

  (check-manager big-map 27 1 0 player-bad-start% 1)
  (check-manager big-map 27 1 0 player-bad-end% 1)
  (check-manager big-map 0 0 1 player-bad-end% 1) ;; <--- this one must become a milestone 10 test
  (check-manager big-map 0 1 0 player-bad-end% 3)
  (check-manager big-map 0 1 0 player-bad-win% 1))

