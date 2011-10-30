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
    [else v]))

; --- helpers

(define (fxzero? x) (fx= x 0))

(define (shift n) (fxrshift n 1))

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
    [else (if (fxzero? i) t #f)]))
;[else (if (fxzero? i) t (error 'lookup-tree "illegal index"))])))

(define (lookup i ls)
  (match ls 
    ['() #f]
    [(cons (cons weight tree) ls*)
     (if (fx< i weight)
       (lookup-tree weight i tree)
       (lookup (fx- i weight) ls*))]))

(define (update-tree w i v t)
  (match t
    [(node t-val t-even t-odd)
     (if (fxzero? i) 
       (node v t-even t-odd)
       (let [(w/2 (shift w))]
         (if (fx<= i w/2)
           (node t-val
                 (update-tree w/2 (fx- i 1) v t-even)
                 t-odd)
           (node t-val
                 t-even
                 (update-tree w/2 (fx- (fx- i 1) w/2) v t-odd)))))]
    [_ (if (fxzero? i) v (error 'update-tree "illegal index"))]))

(define (update i v ls)
  (match ls
    ['()  (error 'k:update "illegal index ~s ~s" i v)]
    [(cons (and t (cons weight tree)) ls*)
     (if (fx< i weight)
       (cons (cons weight (update-tree weight i v tree)) ls*)
       (cons t (update (fx- i weight) v ls*)))]))
