; binary random access list based on skew numbers
#lang racket/base
(require racket/fixnum
         racket/match)
(provide
  k:size k:lookup k:update k:get-value k:empty
  k:new-var
  var?
  )

(struct node (val even odd))
(struct var (name idx))
(struct subst (size bral))

; -- public 

(define k:empty (subst 0 '()))

(define k:size subst-size)

(define (k:new-var name sub)
  (let ([x (var name (subst-size sub))])
    (values x (k:associate x sub))))

(define (k:associate v sub)
  (match sub
    [(subst size bral)
     (subst (fx+ 1 size) (bind v bral))]))

(define (from-tail i sub-size)
  (fx- (fx- sub-size 1) i))

(define (k:lookup v sub)
  (lookup (from-tail (var-idx v) (subst-size sub)) (subst-bral sub)))

(define (k:update a-var v sub)
  (match sub
    [(subst size bral)
     (subst size (update (from-tail (var-idx a-var) size) v bral))]))

(define (k:get-value v)
  (cond
    [(node? v) (node-val v)]
    [else (car v)]))

; --- helpers

(define (fxzero? x) (fx= x 0))

(define shift (lambda (n) (fxrshift n 1)))

(define (bind v ls)
  (match ls
    [(cons (cons x y) (cons (cons a b) c))
     (if (fx= x a)
       (cons (cons (fx+ 1 (fx+ x a))
                   (node v y b))
             c)
       (cons (cons 1 v) ls))]
    [_ (cons (cons 1 v) ls)]))

(define (lookup-tree w i t)
  (cond
    [(node? t)
     (if (fxzero? i) t ;(node-val t)
       (let [(w/2 (shift w))]
         (if (fx<= i w/2)
           (lookup-tree w/2 (fx- i 1) (node-even t))
           (lookup-tree w/2 (fx- (fx- i 1) w/2) (node-odd t)))))]
    [else (if (fxzero? i) (cons t '()) #f)]))
;[else (if (fxzero? i) t (error 'lookup-tree "illegal index"))])))

(define (lookup i ls)
  (cond
    [(null? ls) #f]
    ;[(null? ls) (error 'k:lookup "illegal index")]
    [else (let [(t (car ls))]
            (if (fx< i (car t))
              (lookup-tree (car t) i (cdr t))
              (lookup (fx- i (car t)) (cdr ls))))]))

(define (update-tree w i v t)
  (cond
    [(node? t)
     (if (fxzero? i) (node v (node-even t) (node-odd t))
       (let [(w/2 (shift w))]
         (if (fx<= i w/2)
           (node (node-val t)
                 (update-tree w/2 (fx- i 1) v (node-even t))
                 (node-odd t))
           (node (node-val t)
                 (node-even t)
                 (update-tree w/2 (fx- (fx- i 1) w/2) v (node-odd t))))))]
    [else (if (fxzero? i) v (error 'update-tree "illegal index"))]))

(define (update i v ls)
  (cond
    [(null? ls) (error 'k:update "illegal index ~s ~s" i v)]
    [else (let [(t (car ls))]
            (if (fx< i (car t))
              (cons `(,(car t) . ,(update-tree (car t) i v (cdr t))) (cdr ls))
              (cons t (update (fx- i (car t)) v (cdr ls)))))]))
