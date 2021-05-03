#lang typed/racket/base
(provide
 sbral
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

(require racket/match)

(module+ test (require typed/rackunit)
  (define-syntax thunk
    (syntax-rules ()
      [(_ e) (lambda () e)])))

(define-type (Tree t) (U (leaf t) (branch t)))
(struct (t) leaf ({val : t}))
(struct (t) branch ({val : t} {even : (Tree t)} {odd : (Tree t)}))

(define-type (Forest t) (U empty-forest (cons-forest t)))
(struct empty-forest ())
(struct (t) cons-forest ({weight : Integer} {tree : (Tree t)} {child : (Forest t)}))

(struct (t) sbral ({size : Integer} {forest : (Forest t)}))

(: forest-weight : (All (t) (Forest t) -> Integer))
(define (forest-weight forest)
  (match forest
    [(empty-forest) 0]
    [(cons-forest weight _ child-forest)
     (+ weight (forest-weight child-forest))]))

(: assert-invariant! : (All (t) (sbral t) -> 'ok))
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
(: sbral-empty? : (All (t) (sbral t) -> Boolean))
(define (sbral-empty? s) (fxzero? (sbral-size s)))
(module+ test (check-not-exn (thunk (assert-invariant! sbral-empty))))

(: sbral-cons : (All (t) t (sbral t) -> (sbral t)))
(define (sbral-cons elt s)
  (let ([size : Integer (sbral-size s)]
        [forest : (Forest t) (sbral-forest s)])
    (let ([forest* : (Forest t) (forest-add forest elt)])
      ((inst sbral t) (+ 1 size) forest*))))

(: sbral-head : (All (t) (sbral t) -> t))
(define (sbral-head s) (forest-head (sbral-forest s)))

(: sbral-tail : (All (t) (sbral t) -> (sbral t)))
(define (sbral-tail s)
  (match s
    [(sbral size forest)
     (let ([forest* : (Forest t) (forest-tail forest)])
       (sbral (- size 1) forest*))]))

(: from-tail : Integer Integer -> Integer)
(define (from-tail i sbral-size)
  (- (- sbral-size 1) i))

(: sbral-ref : (All (t) (sbral t) Integer -> (Option t)))
(define (sbral-ref sbral index)
  (forest-ref (sbral-forest sbral)
              (from-tail index (sbral-size sbral))))

(: sbral-set : (All (t) (sbral t) Integer t -> (sbral t)))
(define (sbral-set s index v)
  (match s
    [(sbral size forest)
     (let ([forest* : (Forest t)
                    (forest-set forest 
                                (from-tail index size) 
                                v)])
       (sbral size forest*))]))

(: sbral->reversed-list : (All (t) (sbral t) -> (Listof t)))
(define (sbral->reversed-list sbral)
  (let loop ([forest : (Forest t) (sbral-forest sbral)]
             [elements : (Listof t) '()])
    (match forest
      [(empty-forest) elements]
      [_ (loop (forest-tail forest)
               ((inst cons t (Listof t)) (forest-head forest) elements))])))

(: sbral->list : (All (t) (sbral t) -> (Listof t)))
(define (sbral->list sbral)
  (reverse (sbral->reversed-list sbral)))

(: list->sbral : (All (t) (Listof t) -> (sbral t)))
(define (list->sbral elements)
  (define forest
    (for/fold ([forest : (Forest t) (empty-forest)])
              ([elt (in-list (reverse elements))])
      (forest-add forest elt)))
  (sbral (length elements) forest))

;;
;; Forest & Tree helper functions
;;

(: fxzero? : Integer -> Boolean)
(define (fxzero? x) (zero? x))

(: half : Integer -> Integer)
(define (half n) (arithmetic-shift n -1))

;; Produce a new forest with [v] at the logical "end"
(: forest-add : (All (t) (Forest t) t -> (Forest t)))
(define (forest-add forest elt)
  (match forest
    [(cons-forest w0 tree0 (cons-forest w1 tree1 sub-forest))
     (if (= w0 w1)
         (cons-forest (+ 1 w0 w1)
                      (branch elt tree0 tree1)
                      sub-forest)
         (cons-forest 1 (leaf elt) forest))]
    [_ (cons-forest 1 (leaf elt) forest)]))

(: forest-head : (All (t) (Forest t) -> t))
(define (forest-head forest)
  (match forest
    [(empty-forest)
     (raise-argument-error 'forest-head "nonempty forest" forest)]
    [(cons-forest _1 tree _2)
     (match tree
       [(branch val _1 _2) val]
       [(leaf val) val])]))

(: forest-tail : (All (t) (Forest t) -> (Forest t)))
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
     (let ([w* (half (- weight 1))])
       (when (even? w*) (error 'forest-tail "bug: even w*"))
       (cons-forest w* t-even (cons-forest w* t-odd sub-forest)))]))

(: tree-ref : (All (a) (Tree a) Integer Integer -> (Option a)))
(define (tree-ref t w i)
  (cond
    [(branch? t)
     (if (fxzero? i) 
         (branch-val t)
         (let [(w/2 (half w))]
           (if (<= i w/2)
               (tree-ref (branch-even t) w/2 (- i 1))
               (tree-ref (branch-odd t)  w/2 (- (- i 1) w/2)))))]
    [else
     (if (fxzero? i) (leaf-val t) #f)]))

(: forest-ref : (All (t) (Forest t) Integer -> (Option t)))
(define (forest-ref forest i)
  (match forest
    [(empty-forest) #f]
    [(cons-forest weight tree sub-forest)
     (if (< i weight)
         (tree-ref tree weight i)
         (forest-ref sub-forest (- i weight)))]))

(: tree-set : (All (t) (Tree t) Integer Integer t -> (Tree t)))
(define (tree-set t w i v)
  (match t
    [(branch t-val t-even t-odd)
     (if (fxzero? i) 
         (branch v t-even t-odd)
         (let [(w/2 (half w))]
           (if (<= i w/2)
               (branch t-val
                       (tree-set t-even w/2 (- i 1) v)
                       t-odd)
               (branch t-val
                       t-even
                       (tree-set t-odd w/2 (- (- i 1) w/2) v)))))]
    [_ (if (fxzero? i)
           (leaf v)
           ;; CR dalev: throw range error
           (raise-argument-error 'tree-set "index too big" i))]))

(: forest-set : (All (t) (Forest t) Integer t -> (Forest t)))
(define (forest-set forest i v)
  (match forest
    [(empty-forest) 
     ;; CR dalev: throw range error
     (raise-argument-error 'forest-set "index too big" i)]
    [(cons-forest weight tree sub-forest)
     (if (< i weight)
         (cons-forest weight 
                      (tree-set tree weight i v) 
                      sub-forest)
         (cons-forest weight 
                      tree 
                      (forest-set sub-forest (- i weight) v)))]))

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
      (branch 'b 
              (branch 'c (leaf 'd) (leaf 'e)) 
              (branch 'f (leaf 'g) (leaf 'h)))
      (branch 'i 
              (branch 'j (leaf 'k) (leaf 'l)) 
              (branch 'm (leaf 'n) (leaf 'o))))
     (empty-forest)))
  
  (define sbral-15 (sbral 15 a-forest))
  
  (check-not-exn (thunk (assert-invariant! sbral-15)))
  
  (check-equal? (sbral-ref sbral-15 12) 'c)
  (check-equal? (sbral-head sbral-15) 'a)
  (check-equal? (sbral-head (sbral-tail sbral-15)) 'b)
  
  (check-equal? (sbral->list sbral-15)
                '(a b c d e f g h i j k l m n o))
  
  (define elements (sbral->list sbral-15))
  
  (: sbral=? : (All (t) (-> (sbral t) (sbral t) Boolean)))
  (define (sbral=? a b)
    (and (= (sbral-size a) (sbral-size b))
         (for/and ([i (in-range (sbral-size a))])
           (equal? (sbral-ref a i) (sbral-ref b i)))))
  
  (let ([new-sbral-15 (list->sbral (sbral->list sbral-15))])
    (check-equal? (sbral-size sbral-15) (sbral-size new-sbral-15))
    (check-true (sbral=? new-sbral-15 sbral-15)))
  
  (check-equal? (sbral->list (list->sbral elements))
                elements)
  )
