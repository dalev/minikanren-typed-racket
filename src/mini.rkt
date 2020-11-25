#lang typed/racket/base
(provide 
  fresh
  conde
  run
  run*
  conj
  disj)

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

