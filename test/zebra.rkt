#lang racket/base
(require "../main.rkt")

(define (nullo x) (== '() x))

(define membo
  (lambda (elt ls)
    (fresh (d a)
      (conde
        ((nullo ls) fail)
        ((== (cons elt d) ls))
        ((== (cons a d) ls) (membo elt d))))))

(define on-righto
  (lambda (e1 e2 ls)
    (fresh (d a r)
      (conde
        ((nullo ls) fail)
        ((== (cons a '()) ls) fail)
        ((== (cons e1 d) ls) (== (cons e2 r) d))
        ((== (cons a d) ls) (on-righto e1 e2 d))))))

(define next-too
  (lambda (e1 e2 ls)
    (conde
      ((on-righto e1 e2 ls))
      ((on-righto e2 e1 ls)))))

(define zebrao
  (lambda ()
    (run* (h)
      (fresh (a1 a2 a3 a4 a5
                 b1 b2 b3 b4 b5
                 c1 c2 c3 c4 c5
                 d1 d2 d3 d4 d5
                 e1 e2 e3 e4 e5)
        (== h `((,a1 ,a2 ,a3 ,a4 ,a5)
                (,b1 ,b2 ,b3 ,b4 ,b5)
                (,c1 ,c2 ,c3 ,c4 ,c5)
                (,d1 ,d2 ,d3 ,d4 ,d5)
                (,e1 ,e2 ,e3 ,e4 ,e5)))
        (== a1 'norwegian)
        (== c3 'milk)
        (fresh (t1 t2 t3)
          (membo `(englishman ,t1 ,t2 ,t3 red) h))
        (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
          (on-righto `(,t1 ,t2 ,t3 ,t4 ivory)
                     `(,t5 ,t6 ,t7 ,t8 green) h))
        (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
          (next-too `(norwegian ,t1 ,t2 ,t3 ,t4)
                    `(,t5 ,t6 ,t7 ,t8 blue) h))
        (fresh (t1 t2 t3)
          (membo `(,t1 kools ,t2 ,t3 yellow) h))
        (fresh (t1 t2 t3)
          (membo `(spaniard ,t1 ,t2 dog ,t3) h))
        (fresh (t1 t2 t3)
          (membo `(,t1 ,t2 coffee ,t3 green) h))
        (fresh (t1 t2 t3)
          (membo `(ukrainian ,t1 tea ,t2 ,t3) h))
        (fresh (t1 t2 t3)
          (membo `(,t1 luckystrikes oj ,t2 ,t3) h))
        (fresh (t1 t2 t3)
          (membo `(japanese parliaments ,t1 ,t2 ,t3) h))
        (fresh (t1 t2 t3)
          (membo `(,t1 oldgolds ,t2 snails ,t3) h))
        (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
          (next-too `(,t1 ,t2 ,t3 horse ,t4) 
                    `(,t5 kools ,t6 ,t7 ,t8) h))
        (fresh (t1 t2 t3 t4 t5 t6 t7 t8)
          (next-too `(,t1 ,t2 ,t3 fox ,t4)
                    `(,t5 chesterfields ,t6 ,t7 ,t8) h))
        (fresh (t1 t2 t3 t4)
          (membo `(,t1 ,t2 water ,t3 ,t4) h)) ;?
        (fresh (t1 t2 t3 t4)
          (membo `(,t1 ,t2 ,t3 zebra ,t4) h)))))) ;?

(module+ test
  (require rackunit)
  (define expected-solution
    '(((norwegian kools water fox yellow)
       (ukrainian chesterfields tea horse blue)
       (englishman oldgolds milk snails red)
       (spaniard luckystrikes oj dog ivory)
       (japanese parliaments coffee zebra green))))

  (check-equal? (zebrao) expected-solution))

(module+ main
  (define n 500)
  (printf "Timing ~a runs...~n" n)
  (define-values {_answers cpu real gc}
    (time-apply 
      (lambda ()
        (for ([_ (in-range 0 n)])
          (zebrao))) 
      '()))

  (let ([n (exact->inexact n)])
    (printf "Per run: cpu: ~a ms, real: ~a ms, gc: ~a ms~n"
            (/ cpu n) 
            (/ real n) 
            (/ gc n))))

