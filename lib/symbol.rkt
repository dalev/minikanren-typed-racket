#lang racket/base
(provide [rename-out (symbol-append append)])

(define (symbol-append s t)
  (string->symbol (string-append (symbol->string s) (symbol->string t))))
