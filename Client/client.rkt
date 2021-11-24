#lang racket

;; a client that creates players and signs them up some with a server
;; at a given IP address and port 

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
                        
(require Trains/Common/player-interface)
(require (only-in SwDev/Testing/make-client port/c))

(provide
 (contract-out
  [client 
   #; (client players ip port# wait?)
   ;; runs a client that connects all players to a server at ip on port#
   ;; waits for all of them if wait? is #t -- NEEDED FOR INDEPENDENT
   ;; RUNS of the client in a shell process 
   (->* ([listof manager-player/c]) (port/c boolean? string? #:quiet boolean?) any)]))

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

(require Trains/Remote/manager)
(require Trains/Server/basic-constants)
(require (except-in SwDev/Testing/make-client port/c))
(require SwDev/Testing/communication)

;                                                  
;                                                  
;            ;;;       ;                           
;              ;                              ;    
;              ;                              ;    
;     ;;;      ;     ;;;     ;;;;   ; ;;;   ;;;;;; 
;    ;   ;     ;       ;    ;    ;  ;;   ;    ;    
;   ;          ;       ;    ;;;;;;  ;    ;    ;    
;   ;          ;       ;    ;       ;    ;    ;    
;   ;          ;       ;    ;       ;    ;    ;    
;    ;   ;     ;       ;    ;;   ;  ;    ;    ;    
;     ;;;       ;;;  ;;;;;   ;;;;;  ;    ;     ;;; 
;                                                  
;                                                  
;                                                  
;                                                  

(define LOCAL "127.0.0.1")
(define PORT0 45678)

(define (client players (port PORT0) (wait? #false) (ip LOCAL) #:quiet (quiet #true))
  (define (connector name)
    (if (not name)
        (connect-to-server-as-receiver ip port)
        (connect-to-server-as-receiver ip port #:init (λ (ip) (send-message name ip)))))
  (define player-threads (make-players wait? players connector quiet))
  (when wait?
    (wait-for-all player-threads)
    (displayln "all done")))

#; {type ChanneledThreads = [Listof [List Channel Thread]]}

#; {Boolean [Listof Player] [-> (values InputPort OutputPort)] Boolean -> ChanneledThreads}
(define (make-players wait? players connector quiet)
  (define done (make-channel))
  (for/list ((1player players) (i (in-naturals)))
    (define-values (name behavior manager)
      (let ()
        (define name (get-field name 1player))
        (define-values (receiver _) (connector name))
        (values name 1player (make-remote-manager receiver))))
    (list done
          (thread
           (λ ()
             (define err-out (if quiet (open-output-string) (current-error-port)))
             (parameterize ([prefix-with-spaces 5000]
                            [current-error-port err-out]
                            [trickle-output? #t])
               (define r (manager behavior))
               (if wait? (channel-put done (list name r)) (void))))))))

#; {ChanneledThreads -> Void}
;; display the results 
(define (wait-for-all player-threads)
  (when (cons? player-threads)
    (define removes-itself
      (for/list ((dp player-threads))
        (match-define [list done th] dp)
        (handle-evt done (λ (r) (log-info "~a" r) (wait-for-all (remq dp player-threads))))))
    (apply sync removes-itself)))
