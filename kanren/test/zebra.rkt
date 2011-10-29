#lang racket/base
(require "../main.rkt")
(provide zebrao test-zebra)

(define (nullo x) (== '() x))

(define membo
  (lambda (elt ls)
    (exist (d a)
      (conde
        ((nullo ls) fail)
        ((== (cons elt d) ls))
        ((== (cons a d) ls) (membo elt d))))))

(define on-righto
  (lambda (e1 e2 ls)
    (exist (d a r)
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

(define test-zebra
  (lambda (n)
    (cond
      ((zero? n) '())
      (else (begin
              (zebrao)
              (test-zebra (sub1 n)))))))

(define zebrao
  (lambda ()
    (run* (h)
         (exist (a1 a2 a3 a4 a5
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
                (exist (t1 t2 t3)
                       (membo `(englishman ,t1 ,t2 ,t3 red) h))
                (exist (t1 t2 t3 t4 t5 t6 t7 t8)
                  (on-righto `(,t1 ,t2 ,t3 ,t4 ivory)
                             `(,t5 ,t6 ,t7 ,t8 green) h))
                (exist (t1 t2 t3 t4 t5 t6 t7 t8)
                  (next-too `(norwegian ,t1 ,t2 ,t3 ,t4)
                            `(,t5 ,t6 ,t7 ,t8 blue) h))
                (exist (t1 t2 t3)
                       (membo `(,t1 kools ,t2 ,t3 yellow) h))
                (exist (t1 t2 t3)
                       (membo `(spaniard ,t1 ,t2 dog ,t3) h))
                (exist (t1 t2 t3)
                       (membo `(,t1 ,t2 coffee ,t3 green) h))
                (exist (t1 t2 t3)
                       (membo `(ukrainian ,t1 tea ,t2 ,t3) h))
                (exist (t1 t2 t3)
                       (membo `(,t1 luckystrikes oj ,t2 ,t3) h))
                (exist (t1 t2 t3)
                       (membo `(japanese parliaments ,t1 ,t2 ,t3) h))
                (exist (t1 t2 t3)
                       (membo `(,t1 oldgolds ,t2 snails ,t3) h))
                (exist (t1 t2 t3 t4 t5 t6 t7 t8)
                  (next-too `(,t1 ,t2 ,t3 horse ,t4) 
                            `(,t5 kools ,t6 ,t7 ,t8) h))
                (exist (t1 t2 t3 t4 t5 t6 t7 t8)
                  (next-too `(,t1 ,t2 ,t3 fox ,t4)
                            `(,t5 chesterfields ,t6 ,t7 ,t8) h))
                (exist (t1 t2 t3 t4)
                       (membo `(,t1 ,t2 water ,t3 ,t4) h)) ;?
                (exist (t1 t2 t3 t4)
                       (membo `(,t1 ,t2 ,t3 zebra ,t4) h)))))) ;?

