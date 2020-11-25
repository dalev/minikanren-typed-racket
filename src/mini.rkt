#lang racket/base
(provide 
  fresh
  conde
  run
  run*
  conj
  disj
  fail)

(require "micro.rkt")

(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s c) (lambda () (g s c))))))

(define-syntax conj
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj2 (Zzz g0) (conj g ...)))))

(define-syntax disj
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj2 (Zzz g0) (disj g ...)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh
      (lambda (x0)
        (fresh (x ...) g0 g ...))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj (conj g0 g ...) ...))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (map reify-1st (take n (call/goal (fresh (x ...) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (map reify-1st (take-all (call/goal (fresh (x ...) g0 g ...)))))))

(define (call/goal g) (g sbral-empty 0))

(define (pull $)
  (if (procedure? $) (pull ($)) $))

(define (take-all $)
  (let (($ (pull $)))
    (if (null? $) '() (cons (car $) (take-all (cdr $))))))

(define (take n $)
  (if (zero? n) '()
    (let (($ (pull $)))
      (if (null? $) '() (cons (car $) (take (- n 1) (cdr $)))))))

