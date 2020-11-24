#lang racket/base
(require racket/fixnum
         racket/match)
(provide
  sbral?
  sbral-empty?
  sbral-empty
  sbral-cons
  sbral-head
  sbral-tail
  sbral-set
  sbral-ref
  sbral-size
  
  sbral->list
  list->sbral)

(module+ test (require rackunit racket/function))

(module internal-structures racket/base
  ;; CR dalev: it seems like merely implementing the gen:equal+hash interface
  ;; on these structures causes the zebra benchmark to slow down 37%!
  ;; Investigate this more -- I'm pretty sure the unifier does not suddenly
  ;; start comparing or hashing substitutions.
  (require racket/match racket/fixnum)
  (provide (struct-out branch)
           (struct-out empty-forest)
           (struct-out cons-forest)
           (struct-out sbral))
  (define (branch=? b1 b2 rec-equal?)
    (match* {b1 b2}
      [{(branch v1 e1 o1)
        (branch v2 e2 o2)}
        (and (rec-equal? v1 v2)
             (rec-equal? e1 e2)
             (rec-equal? o1 o2))]))

  (define (branch-hash b rec-hash)
    (match b
      [(branch v even odd)
       (rec-hash (list v even odd))]))

  (define (branch-hash2 b rec-hash) (branch-hash b rec-hash))

  ;; (Tree a) = (branch a (Tree a) (Tree a)) U a
  ;; At the leaves, we just have a non-node value.
  (struct branch (val even odd)
          #:methods gen:equal+hash
          [(define equal-proc branch=?)
           (define hash-proc branch-hash)
           (define hash2-proc branch-hash2)])


  (define (empty-forest=? f1 f2 _)
    (and (empty-forest? f1) (empty-forest? f2)))

  (define empty-forest-hash (equal-hash-code 0))
  (define empty-forest-hash2 (equal-secondary-hash-code 0))

  ;; (Forest a) = (empty-forest) U (cons-forest fixnum? (Tree a) (Forest a))
  (struct empty-forest ()
          #:methods gen:equal+hash
          [(define equal-proc empty-forest=?)
           (define (hash-proc a b) empty-forest-hash)
           (define (hash2-proc a b) empty-forest-hash2)])

  (define (cons-forest=? f1 f2 rec-equal?)
    (match* {f1 f2}
      [{(cons-forest w1 t1 f1*) (cons-forest w2 t2 f2*)}
                                (and (rec-equal? w1 w2)
                                     (rec-equal? t1 t2)
                                     (rec-equal? f1* f2*))]))

  (define (cons-forest-hash f rec-hash)
    (match f
      [(cons-forest weight tree sub-forest)
       (rec-hash (list weight tree sub-forest))]))

  (define (cons-forest-hash2 f rec-hash)
    (cons-forest-hash f rec-hash))

  (struct cons-forest (weight tree child)
          #:methods gen:equal+hash
          [(define equal-proc cons-forest=?)
           (define hash-proc cons-forest-hash)
           (define hash2-proc cons-forest-hash2)])

  (define (sbral=? s1 s2 rec-equal?)
    (and (fx= (sbral-size s1) (sbral-size s2))
         (rec-equal? (sbral-forest s1) (sbral-forest s2))))

  (define (sbral-hash s rec-hash)
    (match s
      [(sbral size forest)
       (rec-hash (list size forest))]))

  (define (sbral-hash2 s rec-hash)
    (sbral-hash s rec-hash))

  ;; (Sbral a) = (sbral fixnum? (Forest a))
  (struct sbral (size forest)
          #:methods gen:equal+hash
          [(define equal-proc sbral=?)
           (define hash-proc sbral-hash)
           (define hash2-proc sbral-hash2)]))

(require 'internal-structures)

(define (forest-weight forest)
  (match forest
    [(empty-forest) 0]
    [(cons-forest weight _ child-forest)
     (+ weight (forest-weight child-forest))]))

(define (assert-invariant! sbral)
  (unless (= (sbral-size sbral) 
             (forest-weight (sbral-forest sbral)))
    (error "sbral-size does not match total forest weight"))
  (let loop ([forest (sbral-forest sbral)])
    (match forest
      [(empty-forest) 'ok]
      [(cons-forest weight _ child-forest)
       (if (even? weight)
         (error "sbral has an even weighted subtree")
         (loop child-forest))])))

(define sbral-empty (sbral 0 (empty-forest)))
(define (sbral-empty? s) (fxzero? (sbral-size s)))
(module+ test (check-not-exn (thunk (assert-invariant! sbral-empty))))

(define (sbral-cons elt s)
  (match s
    [(sbral size forest)
     (sbral (fx+ 1 size) (forest-add forest elt))]))

(define (sbral-head s) (forest-head (sbral-forest s)))

(define (sbral-tail s)
  (match s
    [(sbral size forest)
     (sbral (fx- size 1) (forest-tail forest))]))

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

(define (sbral->reversed-list sbral)
  (let loop ([forest (sbral-forest sbral)]
             [elements '()])
    (match forest
      [(empty-forest) elements]
      [_ (loop (forest-tail forest)
               (cons (forest-head forest) elements))])))

(define (sbral->list sbral)
  (reverse (sbral->reversed-list sbral)))

(define (list->sbral elements)
  (define-values [size forest]
    (for/fold ([acc-size 0]
               [forest (empty-forest)])
      ([elt (in-list (reverse elements))]
       [size (in-naturals 1)])
      (values size (forest-add forest elt))))
  (sbral size forest))

;;
;; Forest & Tree helper functions
;;

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

(define (forest-head forest)
  (match forest
    [(empty-forest)
     (raise-argument-error 'forest-head "nonempty forest" forest)]
    [(cons-forest _ tree _)
     (match tree
       [(branch head-elt _ _) head-elt]
       [_ tree])]))

(define (forest-tail forest)
  (match forest
    [(empty-forest) 
     (raise-argument-error 'forest-tail "nonempty forest" forest)]
    [(cons-forest 1 tree sub-forest)
     (if (branch? tree)
       (error 'forest-tail 
              "ill-formed forest: weight 1 with a non-leaf tree: ~a" 
              forest)
       sub-forest)]
    [(cons-forest weight (branch elt t-even t-odd) sub-forest)
     (when (even? weight) 
       (error 'forest-tail "ill-formed forest: even weight: ~a" forest))
     (let ([w* (half (fx- weight 1))])
       (when (even? w*) (error 'forest-tail "bug: even w*"))
       (cons-forest w* t-even (cons-forest w* t-odd sub-forest)))]))

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
  (check-not-exn (thunk (assert-invariant! sbral-4)))

  (let* ([s sbral-4]
         [s (sbral-set s 0 42)]
         [s (sbral-set s 3 17)]
         [s (sbral-set s 2 (sbral-ref s 3))])

    (check-equal? (sbral-ref s 0) 42)
    (check-equal? (sbral-ref s 1) 'z)
    (check-equal? (sbral-ref s 2) 17)
    (check-equal? (sbral-ref s 3) 17))

  (define a-forest
    (cons-forest
      15
      (branch
        'a
        (branch 'b (branch 'c 'd 'e) (branch 'f 'g 'h))
        (branch 'i (branch 'j 'k 'l) (branch 'm 'n 'o)))
      (empty-forest)))

  (define sbral-15 (sbral 15 a-forest))

  (check-not-exn (thunk (assert-invariant! sbral-15)))

  (check-equal? (sbral-ref sbral-15 12) 'c)
  (check-equal? (sbral-head sbral-15) 'a)
  (check-equal? (sbral-head (sbral-tail sbral-15)) 'b)

  (check-equal? (sbral->list sbral-15)
                '(a b c d e f g h i j k l m n o))
  
  (define elements (sbral->list sbral-15))

  (let ([new-sbral-15 (list->sbral (sbral->list sbral-15))])
    (check-equal? (sbral-size sbral-15) (sbral-size new-sbral-15))
    (check-equal? new-sbral-15 sbral-15))

  (check-equal? (sbral->list (list->sbral elements))
                elements)
  )


