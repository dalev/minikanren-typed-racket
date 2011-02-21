#lang racket/base
(require racket/contract)

(provide/contract
  [split (string? #:on char? . -> . (listof string?))])

(define (split str #:on on)
  (if (string=? str "")
    (list "")
    (let ([last-index (sub1 (string-length str))])
      (define-values (substrings last-match-start)
        (for/fold ([substrings '()]
                   [end-range (add1 last-index)])
          ;; The loop body will not execute when i = -1; end range is exclusive
          ([c (in-string str last-index -1 -1)]
           [i (in-range last-index -1 -1)])
          (if (char=? c on)
            (values (cons (substring str (add1 i) end-range) substrings)
                    i)
            (values substrings end-range))))
      (cons (substring str 0 last-match-start) substrings))))


