#lang racket/base
(require "zebra.rkt" racket/pretty)
(pretty-display (zebrao))
(define n 500)
(printf "Timing ~a runs...~n" n)
(time (test-zebra n))
