#lang racket/base
(require racket/fixnum
         racket/match)
(provide
  (rename-out [subst-empty empty])
  extend
  walk
  create-variable
  var? var=?
  )

(module+ test (require rackunit))

(struct bral-empty ())
(struct bral-node (weight tree rest))

;; Tree branches inside the BRAL:
(struct node (val even odd))
;; At the leaves, we just have a non-node value.

;(define-type (Tree a)
;             (U (node a (Tree a) (Tree a))
;                a))

;; Finally, a substitution is a BRAL that may contain variables
(struct var (name idx))

;; CR dalev: the subst wrapper is superfluous because we should have
;; (subst-size s) = (bral-node-weight (subst-bral s))
(struct subst (size bral))

(define (var=? v w)
  (and (eq? (var-name v) (var-name w))
       (eqv? (var-idx v) (var-idx w))))

(define subst-empty (subst 0 (bral-empty)))

(define (create-variable name sub)
  (match sub
    [(subst size bral)
     (let ([x (var name size)])
       (values x (subst (fx+ 1 size) (insert x bral))))]))

(define (from-tail i sub-size)
  (fx- (fx- sub-size 1) i))

(define (subst-find sub v)
  (lookup (from-tail (var-idx v) (subst-size sub)) (subst-bral sub)))

(define (extend sub a-var v)
  (match sub
    [(subst size bral)
     (subst size (update (from-tail (var-idx a-var) size) v bral))]))

;; Transitive closure of [subst-find]
(define (walk v s)
  (cond
    [(var? v)
     (cond
       [(subst-find s v) 
        => (lambda (a)
             ;; CR dalev: does (eq? a v) suffice here?
             (if (and (var? a) (var=? v a))
               v
               (walk a s)))]
       [else v])]
    [else v]))

; --- helpers

(define (fxzero? x) (fx= x 0))

(define (half n) (fxrshift n 1))

(define (insert v bral)
  (match bral
    [(bral-node w0 tree0 (bral-node w1 tree1 bral*))
     (if (fx= w0 w1)
       (bral-node (fx+ 1 (fx+ w0 w1))
                  (node v tree0 tree1)
                  bral*)
       (bral-node 1 v bral))]
    [_ (bral-node 1 v bral)]))

(define (lookup-tree w i t)
  (cond
    [(node? t)
     (if (fxzero? i) 
       (node-val t)
       (let [(w/2 (half w))]
         (if (fx<= i w/2)
           (lookup-tree w/2 (fx- i 1) (node-even t))
           (lookup-tree w/2 (fx- (fx- i 1) w/2) (node-odd t)))))]
    [else
      ;; [t] is the value sitting at this leaf of the tree.
      (if (fxzero? i) t #f)]))

(define (lookup i ls)
  (match ls 
    [(bral-empty) #f]
    [(bral-node weight tree ls*)
     (if (fx< i weight)
       (lookup-tree weight i tree)
       (lookup (fx- i weight) ls*))]))

(define (update-tree w i v t)
  (match t
    [(node t-val t-even t-odd)
     (if (fxzero? i) 
       (node v t-even t-odd)
       (let [(w/2 (half w))]
         (if (fx<= i w/2)
           (node t-val
                 (update-tree w/2 (fx- i 1) v t-even)
                 t-odd)
           (node t-val
                 t-even
                 (update-tree w/2 (fx- (fx- i 1) w/2) v t-odd)))))]
    [_ (if (fxzero? i) v (error 'update-tree "illegal index"))]))

(define (update i v bral)
  (match bral
    [(bral-empty)  (error 'k:update "illegal index ~s ~s" i v)]
    [(bral-node weight tree bral*)
     (if (fx< i weight)
       (bral-node weight (update-tree weight i v tree) bral*)
       (bral-node weight tree (update (fx- i weight) v bral*)))]))

(module+ test
  (define-values [sub x y z w]
    (let*-values ([(x s) (create-variable 'x subst-empty)]
                  [(y s) (create-variable 'y s)]
                  [(z s) (create-variable 'z s)]
                  [(w s) (create-variable 'w s)])
      (values s x y z w)))

  (check-equal? (subst-size sub) 4)

  (let* ([s sub]
         [s (extend s w 42)]
         [s (extend s x 17)]
         [s (extend s y x)])
    (check-equal? (walk w s) 42)
    (check-equal? (walk x s) 17)
    (check-equal? (walk y s) 17)
    (check-equal? (walk z s) z))

  (let* ([a-node (bral-node
                   15
                   (node
                     'a
                     (node 'b (node 'c 'd 'e) (node 'f 'g 'h))
                     (node 'i (node 'j 'k 'l) (node 'm 'n 'o)))
                   (bral-empty))]
         [a-subst (subst 15 a-node)])
    (check-equal? (walk (var 'arbitrary-name 12) a-subst) 'c))
  )


