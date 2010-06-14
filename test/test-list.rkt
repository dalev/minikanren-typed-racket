#lang scheme
(require "../core.ss")
(require (planet cce/fasttest:3:5/random)
         (planet cce/fasttest:3:5/schemeunit)
         (planet schematics/schemeunit:3:4))

(define numbers (build-list 1000000 (lambda (x) x)))

(check-= (time (list:fold numbers #:init 0 #:f +)) (* 1000000 999999 1/2) 0)
