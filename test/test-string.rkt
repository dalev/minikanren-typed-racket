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

(check-true (string:suffix? "foo.txt" #:suffix ".txt"))
(check-true (string:suffix? "foo.txt" #:suffix ""))
(check-true (string:suffix? "foo.txt" #:suffix "foo.txt"))
(check-false (string:suffix? "foo.txt" #:suffix ".rkt"))
(check-false (string:suffix? "short" #:suffix "blahblahshort"))

(check-equal? (string:chop-suffix "foo.txt" #:suffix ".txt") "foo")
(check-false (string:chop-suffix "foo.txt" #:suffix ".rkt"))
              
