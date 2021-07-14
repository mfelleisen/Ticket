#lang info

(define collection "Trains")
(define pkg-desc "derived from the source for the Fall 2021 Sw Dev project")
(define pkg-authors '(matthias))
(define version "0.1")

(define sw-dev "git://github.com/mfelleisen/SwDev.git")

(define deps
  `("base"
     "net-lib"
     "pict-lib"
     "scribble-lib"
     "typed-racket-lib"
     "scribble-abbrevs"
     "htdp-lib"
     "gregor-lib"
     "gui-lib"
     "racket-doc"
     "profile-lib"
     "rackunit-lib"
     ,sw-dev))

(define build-deps
  `( ,sw-dev
      "gui-lib"
      "data-enumerate-lib"
      "at-exp-lib" 
      "rackunit-lib"))

(define scribblings '(("Docs/Trains.scrbl" ())))
