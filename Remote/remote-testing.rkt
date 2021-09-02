#lang racket

(provide
 test-remote)

(require SwDev/Debugging/diff)
(require (for-syntax syntax/parse))
(require rackunit)
(require json)

(define ((report-diff e) x)
  (cond
    [(and (eq? x (void)) (eq? e (void)))
     ;; special case
     #true]
    [(diff x e)
     =>
     (λ (δ)
       (pretty-print `[actual] (current-error-port))
       (newline (current-error-port))
       (pretty-print x (current-error-port))
       (newline (current-error-port))
       (pretty-print `[expected] (current-error-port))
       (newline (current-error-port))
       (pretty-print e (current-error-port))
       (newline (current-error-port))
       (pretty-print δ (current-error-port))
       #false)]
    [else #t]))
  
(define-syntax (test-remote stx)
  ; #:remote JSexpr ~~ what the client responds with
  ; #:exp JSexpr    ~~ what we expect the method to produce in response
  ; #:msg JSexpr    ~~ what the method sends to the client 
  (syntax-parse stx 
    [(test make-remote-player
           (method args ...)
           (~optional (~seq #:remote input) #:defaults ([input #'(~a "void")]))
           (~optional (~seq #:exp e) #:defaults ([e #'(void)]))
           #:msg m)
     #'(check-pred
        (report-diff m)
        (string->jsexpr
         (with-output-to-string
           (λ ()
             (check-pred ; check-equal?
              (report-diff e)
              (with-input-from-string (jsexpr->string input)
                (λ ()
                  (define ip (current-input-port))
                  (define op (current-output-port))
                  (define r1 (make-remote-player ip op))
                  (send r1 method args ...)))
              (~a "exoected result of call: " m)))))
        (~a "call as message: " m))]))

(module+ test 
  (test-remote 'make-remote-player [play] #:msg "hello"))