#lang racket/base
(require racket/contract)
(provide maybe/c)

(define (maybe/c contract)
  (or/c contract false/c))
