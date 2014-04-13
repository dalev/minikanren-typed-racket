#lang racket/base
(require racket/contract
         racket/string
         "maybe.rkt")

(provide/contract
  [split (string? #:on char? . -> . (listof string?))]
  [lsplit2 (string? #:on char? . -> . (maybe/c (cons/c string? string?)))]
  [rsplit2 (string? #:on char? . -> . (maybe/c (cons/c string? string?)))]
  [concat (->* ((listof string?)) (#:sep string?) string?)]
  [suffix? (string? #:suffix string? . -> . boolean?)]
  [chop-suffix (string? #:suffix string? . -> . (maybe/c string?))]
  [sub (string? #:pos natural-number/c #:len natural-number/c . -> . string?)]
  )

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

(define (suffix? str #:suffix suffix)
  (if (string=? suffix "")
    #t
    (let ([suffix-len (string-length suffix)]
          [str-len (string-length str)])
      (and (<= suffix-len str-len)
           (for/and ([a (in-string suffix (sub1 suffix-len) 0 -1)]
                     [b (in-string str (sub1 str-len) 0 -1)])
             (char=? a b))))))

(define (sub str #:pos pos #:len len)
  (substring str pos (+ pos len)))

(define (chop-suffix str #:suffix suffix)
  (and (suffix? str #:suffix suffix)
       (substring str 0 (- (string-length str)
                           (string-length suffix)))))

