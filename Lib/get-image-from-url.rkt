#lang racket/gui

(provide
 png-from-url)

(require 2htdp/image)
(require net/url)

(define url-christopher "https://www.felleisen.org/Images/christopher.png")
(define uri-for-standadrd-map "file:/Users/matthias/Courses/21SwDev/Source/Images/map.png")

(define (png-from-url url)
  (call/input-url (string->url url)
                  (lambda (url) (get-pure-port url #:redirections 20))
                  (λ (port)
                    (make-object bitmap% port 'unknown #f))))

(module+ test 

  (define aria (png-from-url url-christopher))
  aria

  (define the-map (png-from-url uri-for-standadrd-map))

  (image-width the-map)
  (image-height the-map))
