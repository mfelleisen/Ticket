#lang racket

;; create the README.md files from README.source in the top-level and all code directories 

(define EXCEPTIONS '["scribblings" "Docs"]) ;; no need for organization tables 

(require SwDev/Debugging/spy)

(define (main . x)
  (define show (empty? x))
  (define untracked (git-status-check))
  (define adirs
    (remove* EXCEPTIONS
      (for/list ([fd (directory-list)] #:when (good? untracked fd)) (path->string fd))))
  (readme adirs show)
  (define afils (map (λ (d) (list (build-path d "README.md") d)) adirs))
  (write-readme-and-show (make-header "directory") afils values show))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof PathString] Any -> Void}
(define (readme adirs show)
  (for ([dir adirs]) 
    (parameterize ([current-directory dir])
      (define afils (for*/list ([f (directory-list)] #:when (regexp-match #px"\\.rkt" f)) (list f f)))
      (write-readme-and-show (make-header "file") afils (λ (l) (substring l 3)) show))))

#; {String [Listof [List PathString String]] [String -> String] Any -> Void}
(define (write-readme-and-show header afils clean show)
  (define purps (purpose-statements afils clean))
  (copy-file "README.source" "README.md" 'delete-existing-one)
  (with-output-to-file "README.md"
    #:exists 'append
    (λ () (printf "~a" (make-table header afils purps))))
  (when show (system "open README.md")))

#; {String [Listof [List PathString String]] [Listof String] -> String}
(define (make-table header adirs purps)
  (define content
    (for/list ([d adirs] [p purps])
      (match-define (list dl dn) d)
      (~a "| [" dn "](" dl ")" " | " p " | \n")))
  (apply string-append header content))

#; {[Listof [List PathString String]] -> [Listof String]}
(define (purpose-statements l clean)
  (for/list ([d l])
    (with-handlers ([exn:fail? (lambda (xn) (error 'purpose-statement "~a \n~a" (first d) (exn-message xn)))])
    (with-input-from-file (first d)
      (λ ()
        (clean (string-trim (caddr (port->lines)))))))))

#; {[Path] -> [Setof PathString]}
;; a primitive way to exclude Untracked directories and files 
(define (git-status-check [which-one "./"])
  (parameterize ((current-directory which-one))
    (match-define (list in out pid err control) (process "git status"))
    (define status (port->list read-line in))
    (let loop ((status status))
      (unless (empty? status)
        (define l (first status))
        (cond
          [(regexp-match #px"Untracked" l)
           (let inner ([status (cdddr status)])
             (define next (string-trim (first status)))
             (cond
               [(equal? "" next) '()]
               [else (cons next (inner (rest status)))]))]
          [else (loop (rest status))])))))

#; {[Listof PathString] PathString -> Boolean}
(define (good? untracked fd)
  (and (directory-exists? fd)
       (not (regexp-match #px"\\.|compiled" fd))
       (not (member fd untracked))))

(define (make-header x)
  (string-append
   "\n"
   (string-append "| " x " | purpose |\n")
   "|--------------------- | ------- |\n"))

;; ---------------------------------------------------------------------------------------------------
(module+ main
  (main 'dontshow))
