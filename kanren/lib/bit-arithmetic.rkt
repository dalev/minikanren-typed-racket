#lang racket/base
(require "../main.rkt"
         "list.rkt"
         "util.rkt")

(provide (all-defined-out))

(define build-num
  (lambda (n)
    (cond
      ((zero? n) '())
      ((and (not (zero? n)) (even? n))
       (cons 0
         (build-num (quotient n 2))))
      ((odd? n)
       (cons 1
         (build-num (quotient (- n 1) 2)))))))

(define full-addero
  (lambda (b x y r c)
    (conde
      ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
      ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
      ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
      ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
      ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
      ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
      ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
      ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c))
      (else fail))))

(define poso
  (lambda (n)
    (exist (a d)
      (== `(,a . ,d) n))))

(define >1o
  (lambda (n)
    (exist (a ad dd)
      (== `(,a ,ad . ,dd) n))))

(define addero
  (lambda (d n m r)
    (condi
      ((== 0 d) (== '() m) (== n r))
      ((== 0 d) (== '() n) (== m r)
       (poso m))
      ((== 1 d) (== '() m)
       (addero 0 n '(1) r))
      ((== 1 d) (== '() n) (poso m)
       (addero 0 '(1) m r))
      ((== '(1) n) (== '(1) m)
       (exist (a c)
         (== `(,a ,c) r)
         (full-addero d 1 1 a c)))
      ((== '(1) n) (gen-addero d n m r))
      ((== '(1) m) (>1o n) (>1o r)
       (addero d '(1) n r))
      ((>1o n) (gen-addero d n m r))
      (else fail))))

(define gen-addero
  (lambda (d n m r)
    (exist (a b c e x y z)
      (== `(,a . ,x) n)
      (== `(,b . ,y) m) (poso y)
      (== `(,c . ,z) r) (poso z)
      (alli
        (full-addero d a b c e)
        (addero e x y z)))))

(define +o
  (lambda (n m k)
    (addero 0 n m k)))

(define -o
  (lambda (n m k)
    (+o m k n)))

(define *o
  (lambda (n m p)
    (condi
      ((== '() n) (== '() p))
      ((poso n) (== '() m) (== '() p))  
      ((== '(1) n) (poso m) (== m p))   
      ((>1o n) (== '(1) m) (== n p))
      ((exist (x z)
         (== `(0 . ,x) n) (poso x)
         (== `(0 . ,z) p) (poso z)
         (>1o m)
         (*o x m z)))
      ((exist (x y)
         (== `(1 . ,x) n) (poso x)
         (== `(0 . ,y) m) (poso y)
         (*o m n p)))
      ((exist (x y)
          (== `(1 . ,x) n) (poso x)      
          (== `(1 . ,y) m) (poso y)
          (odd-*o x n m p)))
      (else fail))))

(define odd-*o
  (lambda (x n m p)
    (exist (q)
      (bound-*o q p n m)
      (*o x m q)
      (+o `(0 . ,q) m p))))

(define bound-*o
  (lambda (q p n m)
    (conde
      ((%null q) (%pair p))
      (else
        (exist (x y z)
          (%cdr q x)
          (%cdr p y)
          (condi
            ((%null n)
             (%cdr m z)
             (bound-*o x y z '()))
            (else
              (%cdr n z) 
              (bound-*o x y z m))))))))

(define =lo
  (lambda (n m)
    (conde
      ((== '() n) (== '() m))
      ((== '(1) n) (== '(1) m))
      (else
        (exist (a x b y)
          (== `(,a . ,x) n) (poso x)
          (== `(,b . ,y) m) (poso y)
          (=lo x y))))))

(define <lo
  (lambda (n m)
    (conde
      ((== '() n) (poso m))
      ((== '(1) n) (>1o m))
      (else
        (exist (a x b y)
          (== `(,a . ,x) n) (poso x)
          (== `(,b . ,y) m) (poso y)
          (<lo x y))))))

(define <=lo
  (lambda (n m)
    (condi
      ((=lo n m) succeed)
      ((<lo n m) succeed)
      (else fail))))

(define <o
  (lambda (n m)
    (condi
      ((<lo n m) succeed)
      ((=lo n m)
       (exist (x)
         (poso x)
         (+o n x m)))
      (else fail))))

(define <=o
  (lambda (n m)
    (condi
      ((== n m) succeed)
      ((<o n m) succeed)
      (else fail))))

(define /o
  (lambda (n m q r)
    (condi
      ((== r n) (== '() q) (<o n m))
      ((== '(1) q) (=lo n m) (+o r m n)
       (<o r m))
      (else
        (alli
          (<lo m n)                        
          (<o r m)                        
          (poso q)                 
          (exist (nh nl qh ql qlm qlmr rr rh)
            (alli
              (splito n r nl nh)
              (splito q r ql qh)
              (conde
                ((== '() nh)
                 (== '() qh)
                 (-o nl r qlm)
                 (*o ql m qlm))
                (else
                  (alli 
                    (poso nh)
                    (*o ql m qlm)
                    (+o qlm r qlmr)
                    (-o qlmr nl rr)
                    (splito rr r '() rh)
                    (/o nh m qh rh)))))))))))

(define splito
  (lambda (n r l h)
    (condi
      ((== '() n) (== '() h) (== '() l))
      ((exist (b n^)
         (== `(0 ,b . ,n^) n)
         (== '() r)
         (== `(,b . ,n^) h)
         (== '() l)))
      ((exist (n^)
         (==  `(1 . ,n^) n)
         (== '() r)
         (== n^ h)
         (== '(1) l)))
      ((exist (b n^ a r^)
         (== `(0 ,b . ,n^) n)
         (== `(,a . ,r^) r)
         (== '() l)
         (splito `(,b . ,n^) r^ '() h)))
      ((exist (n^ a r^)
         (== `(1 . ,n^) n)
         (== `(,a . ,r^) r)
         (== '(1) l)
         (splito n^ r^ '() h)))
      ((exist (b n^ a r^ l^)
         (== `(,b . ,n^) n)
         (== `(,a . ,r^) r)
         (== `(,b . ,l^) l)
         (poso l^)
         (splito n^ r^ l^ h)))
      (else fail))))

(define logo
  (lambda (n b q r)
    (condi
      ((== '(1) n) (poso b) (== '() q) (== '() r))
      ((== '() q) (<o n b) (+o r '(1) n))
      ((== '(1) q) (>1o b) (=lo n b) (+o r b n))
      ((== '(1) b) (poso q) (+o r '(1) n))
      ((== '() b) (poso q) (== r n))
      ((== '(0 1) b)
       (exist (a ad dd)
         (poso dd)
         (== `(,a ,ad . ,dd) n)
         (exp2 n '() q)
         (exist (s)
           (splito n dd r s))))
      ((exist (a ad add ddd)
         (conde
           ((== '(1 1) b))
           (else (== `(,a ,ad ,add . ,ddd) b))))
       (<lo b n)
       (exist (bw1 bw nw nw1 ql1 ql s)
         (exp2 b '() bw1)
         (+o bw1 '(1) bw)
         (<lo q n)
         (exist (q1 bwq1)
           (+o q '(1) q1)
           (*o bw q1 bwq1)
           (<o nw1 bwq1))
           (exp2 n '() nw1)
           (+o nw1 '(1) nw)
           (/o nw bw ql1 s)
           (+o ql '(1) ql1)
         (conde
           ((== q ql))
           (else (<lo ql q)))
         (exist (bql qh s qdh qd)
           (repeated-mul b ql bql)        
           (/o nw bw1 qh s)                
           (+o ql qdh qh)
           (+o ql qd q)
           (conde
             ((== qd qdh))
             (else (<o qd qdh)))
           (exist (bqd bq1 bq)
             (repeated-mul b qd bqd)        
             (*o bql bqd bq)                
             (*o b bq bq1)                
             (+o bq r n)
             (<o n bq1)))))
      (else fail))))

(define exp2
  (lambda (n b q)
    (condi
      ((== '(1) n) (== '() q))
      ((>1o n) (== '(1) q)
       (exist (s)
         (splito n b s '(1))))
      ((exist (q1 b2)                        
         (alli                 
           (== `(0 . ,q1) q)
           (poso q1)
           (<lo b n)
           (%append b `(1 . ,b) b2)
           (exp2 n b2 q1))))
      ((exist (q1 nh b2 s)                
          (alli
            (== `(1 . ,q1) q)
            (poso q1)
            (poso nh)
            (splito n b s nh)
            (%append b `(1 . ,b) b2)
            (exp2 nh b2 q1))))
      (else fail))))

(define repeated-mul
  (lambda (n q nq)
    (conde
      ((poso n) (== '() q) (== '(1) nq))
      ((== '(1) q) (== n nq))
      ((>1o q)
       (exist (q1 nq1)
         (+o q1 '(1) q)
         (repeated-mul n q1 nq1)
         (*o nq1 n nq)))
      (else fail))))

(define expo
  (lambda (b q n)
    (logo n b q '())))

