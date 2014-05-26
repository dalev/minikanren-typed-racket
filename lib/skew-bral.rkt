#lang racket/base
(require racket/fixnum
         racket/match)
(provide
  sbral-empty
  sbral-cons
  sbral-set
  sbral-ref
  sbral-size)

(module+ test (require rackunit))

;; (Tree a) = (branch a (Tree a) (Tree a)) U a
;; At the leaves, we just have a non-node value.
(struct branch (val even odd))

;; (Forest a) = (empty-forest) U (cons-forest fixnum? (Tree a) (Forest a))
(struct empty-forest ())
(struct cons-forest (weight tree child))

;; (Sbral a) = (sbral fixnum? (Forest a))
(struct sbral (size forest))

(define (forest-weight forest)
  (match forest
    [(empty-forest) 0]
    [(cons-forest weight _ child-forest)
     (+ weight (forest-weight child-forest))]))

(define (invariant? sbral)
  (= (sbral-size sbral) 
     (forest-weight (sbral-forest sbral))))

(define sbral-empty (sbral 0 (empty-forest)))
(module+ test (check-true (invariant? sbral-empty)))

(define (sbral-cons elt s)
  (match s
    [(sbral size forest)
     (sbral (fx+ 1 size) (forest-add forest elt))]))

(define (from-tail i sbral-size)
  (fx- (fx- sbral-size 1) i))

(define (sbral-ref sbral index)
  (forest-ref (sbral-forest sbral)
              (from-tail index (sbral-size sbral))))

(define (sbral-set s index v)
  (match s
    [(sbral size forest)
     (sbral size (forest-set forest 
                             (from-tail index size) 
                             v))]))

; --- helpers

(define (fxzero? x) (fx= x 0))

(define (half n) (fxrshift n 1))

;; Produce a new forest with [v] at the logical "end"
(define (forest-add forest elt)
  (match forest
    [(cons-forest w0 tree0 (cons-forest w1 tree1 sub-forest))
     (if (fx= w0 w1)
       (cons-forest (fx+ 1 (fx+ w0 w1))
                    (branch elt tree0 tree1)
                    sub-forest)
       (cons-forest 1 elt forest))]
    [_ (cons-forest 1 elt forest)]))

(define (tree-ref t w i)
  (cond
    [(branch? t)
     (if (fxzero? i) 
       (branch-val t)
       (let [(w/2 (half w))]
         (if (fx<= i w/2)
           (tree-ref (branch-even t) w/2 (fx- i 1))
           (tree-ref (branch-odd t)  w/2 (fx- (fx- i 1) w/2)))))]
    [else
      ;; [t] is the value sitting at this leaf of the tree.
      (if (fxzero? i) t #f)]))

(define (forest-ref forest i)
  (match forest
    [(empty-forest) #f]
    [(cons-forest weight tree sub-forest)
     (if (fx< i weight)
       (tree-ref tree weight i)
       (forest-ref sub-forest (fx- i weight)))]))

(define (tree-set t w i v)
  (match t
    [(branch t-val t-even t-odd)
     (if (fxzero? i) 
       (branch v t-even t-odd)
       (let [(w/2 (half w))]
         (if (fx<= i w/2)
           (branch t-val
                   (tree-set t-even w/2 (fx- i 1) v)
                   t-odd)
           (branch t-val
                   t-even
                   (tree-set t-odd w/2 (fx- (fx- i 1) w/2) v)))))]
    [_ (if (fxzero? i)
         v 
         ;; CR dalev: throw range error
         (raise-argument-error 'tree-set "index too big" i))]))

(define (forest-set forest i v)
  (match forest
    [(empty-forest) 
     ;; CR dalev: throw range error
     (raise-argument-error 'forest-set "index too big" i)]
    [(cons-forest weight tree sub-forest)
     (if (fx< i weight)
       (cons-forest weight 
                    (tree-set tree weight i v) 
                    sub-forest)
       (cons-forest weight 
                    tree 
                    (forest-set sub-forest (fx- i weight) v)))]))

(module+ test
  (define sbral-4 
    (sbral-cons 'x (sbral-cons 'y (sbral-cons 'z (sbral-cons 'w sbral-empty)))))

  (check-equal? (sbral-size sbral-4) 4)
  (check-true (invariant? sbral-4))

  (let* ([s sbral-4]
         [s (sbral-set s 0 42)]
         [s (sbral-set s 3 17)]
         [s (sbral-set s 2 (sbral-ref s 3))])

    (check-equal? (sbral-ref s 0) 42)
    (check-equal? (sbral-ref s 1) 'z)
    (check-equal? (sbral-ref s 2) 17)
    (check-equal? (sbral-ref s 3) 17))

  (let* ([a-forest (cons-forest
                   15
                   (branch
                     'a
                     (branch 'b (branch 'c 'd 'e) (branch 'f 'g 'h))
                     (branch 'i (branch 'j 'k 'l) (branch 'm 'n 'o)))
                   (empty-forest))]
         [a-sbral (sbral 15 a-forest)])
    (check-equal? (sbral-ref a-sbral 12) 'c))
  )


