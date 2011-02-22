#lang racket/base
(require racket/contract
         racket/string)

(provide/contract
  [split (string? #:on char? . -> . (listof string?))]
  [lsplit2 (string? #:on char? . -> . (or/c (cons/c string? string?) false/c))]
  [rsplit2 (string? #:on char? . -> . (or/c (cons/c string? string?) false/c))]
  [concat (->* ((listof string?)) (#:sep string?) string?)])

(define (lsplit2 str #:on on)
  (define on-idx
    (for/first ([c (in-string str)]
                [i (in-naturals)]
                #:when (char=? c on))
      i))
  (and on-idx
       (cons (substring str 0 on-idx)
             (substring str (add1 on-idx)))))

(define (rsplit2 str #:on on)
  (if (string=? str "")
    #f
    (let ([last-index (sub1 (string-length str))])
      (define on-idx
        (for/first ([c (in-string str last-index -1 -1)]
                    [i (in-range last-index -1 -1)]
                    #:when (char=? c on))
                   i))
      (and on-idx
           (cons (substring str 0 on-idx)
                 (substring str (add1 on-idx)))))))
    
(define (split str #:on on)
  (if (string=? str "")
    (list "")
    (let ([last-index (sub1 (string-length str))])
      (define-values (substrings last-match-start)
        (for/fold ([substrings '()]
                   [end-range (add1 last-index)])
          ([c (in-string str last-index -1 -1)]
           [i (in-range last-index -1 -1)])
          ;; The loop body will not execute when i = -1; end range is exclusive
          (if (char=? c on)
            (values (cons (substring str (add1 i) end-range) 
                          substrings)
                    i)
            (values substrings end-range))))
      (cons (substring str 0 last-match-start) substrings))))

(define (concat strings #:sep [sep ""])
  (string-join strings sep))
