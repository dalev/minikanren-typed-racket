#lang racket
(require (planet cce/fasttest/random))

(for ([n-rows (in-range 0 500000)])
  (for ([n-columns (in-range 0 19)])
    (display (random-string #:len (random-natural/poisson 10)))
    (display #\,))
  (display (random-string))
  (newline))
