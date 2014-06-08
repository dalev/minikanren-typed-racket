#lang racket/base
(require "kanren.rkt"
         "list.rkt")

(provide %anywhere
         %sexp-mem
         %find
         %find*)

(define (%anywhere sexp %pred)
  (conde
    [(%pred sexp) succeed]
    [succeed
      (fresh (fst snd)
        (%cons sexp fst snd)
        (any (%anywhere fst %pred)
             (%anywhere snd %pred)))]))

(define (%sexp-mem sexp elt)
  (conde 
    [(%null sexp) (%null elt)]
    [(== sexp elt) succeed]
    [succeed
     (fresh (fst snd)
       (%cons sexp fst snd)
       (any (%sexp-mem fst elt)
            (%sexp-mem snd elt)))]))

(define (%find alist key value)
  (%member alist (list key value)))

(define (%find* nested-record keys value)
  (conde
    [(%null keys) (== value nested-record)]
    [succeed
      (fresh (key1 rest-keys)
        (%cons keys key1 rest-keys)
        (fresh (sub-record)
          (%find nested-record key1 sub-record)
          (%find* sub-record rest-keys value)))]))

(module+ test
  (require (except-in rackunit fail))
  (define (second xs) (car (cdr xs)))

  (check-equal?
    (run* (x) (%find '((k v1) (k v2) (k v3)) 'k x))
    (list 'v1 'v2 'v3))

  (check-equal?
    (run* (x) (%find* '((k v1) (k v2) (k v3)) '(k) x))
    (list 'v1 'v2 'v3))

  (define big-sexp
    '((entry ((name top-1)
              (children
                ((foo ((name foo-1)))
                 (foo ((name foo-2)))
                 (bar ((name bar-1)))))))

      (entry ((name top-2)
              (children ())))

      (entry ((name top-3)
              (children 
                ((bar ((name bar-2)))
                 (foo ((name foo-3)))))))))

  (check-equal?
    (run* (rhs) (%find big-sexp 'entry rhs))
    (for/list ([entry (in-list big-sexp)])
      (second entry)))

  (let ([key 'entry])
    (check-equal?
      (run* (rhs) (%find* big-sexp (list key) rhs))
      (run* (rhs) (%find big-sexp key rhs))))

  (check-equal?
    (run* (name) (%find* big-sexp '(entry name) name))
    (list 'top-1 'top-2 'top-3))

  (define (%child-foo-name table name)
    (%find* table '(entry children foo name) name))

  (check-equal?
    (run* (name) (%child-foo-name big-sexp name))
    '(foo-1 foo-2 foo-3))
  )
