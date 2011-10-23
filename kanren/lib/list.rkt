#lang racket/base
(require "../main.rkt")

(provide (all-defined-out))

(define %car
  (lambda (p a)
    (exist (d)
      (== (cons a d) p))))

(define %cdr
  (lambda (p d)
    (exist (a)
      (== (cons a d) p))))

(define %cons
  (lambda (a d p)
    (== (cons a d) p)))

(define %null
  (lambda (x)
    (== '() x)))

(define %pair
  (lambda (p)
    (exist (a d)
      (%cons a d p))))

(define %list
  (lambda (l)
    (conde
      ((%null l) succeed)
      ((%pair l)
       (exist (d)
         (%cdr l d)
         (%list d)))
      (else fail))))

(define %member
  (lambda (x l)
    (conde
      ((%null l) fail)
      ((%car l x) succeed)
      (else
        (exist (d)
          (%cdr l d)
          (%member x d))))))

(define rembero  
  (lambda (x l out)
    (conde
      ((%null l) (== '() out))
      ((%car l x) (%cdr l out))
      (else (exist (a d res)
              (%cons a d l)
              (rembero x d res)
              (%cons a res out))))))

(define %append
  (lambda (l s out)
    (conde
      ((%null l) (== s out))
      (else 
        (exist (a d res)
          (%cons a d l)
          (%cons a res out)
          (%append d s res))))))

