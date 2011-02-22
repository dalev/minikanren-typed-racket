#lang racket
(require (prefix-in string: "../lib/string.rkt")
         rackunit)

(check-equal? (string:split "a,b,c" #:on #\,)
              (list "a" "b" "c"))

(check-equal? (string:split "foo" #:on #\,) (list "foo"))
(check-equal? (string:split "" #:on #\tab) (list ""))

(check-equal? (string:lsplit2 "a,b,c" #:on #\,)
              (cons "a" "b,c"))

(check-false (string:lsplit2 "a,b,c" #:on #\tab))

(check-equal? (string:rsplit2 "a,b,c" #:on #\,)
              (cons "a,b" "c"))

(check-false (string:rsplit2 "a,b,c" #:on #\tab))

