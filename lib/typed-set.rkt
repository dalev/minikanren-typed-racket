#lang typed-scheme
(require scheme/match)

(provide create mem size of-list to-list Comparator)

(define-type-alias (Comparator a) (a a -> (U 'less 'equal 'greater)))

;; CR dalev: implement balanced trees
(define-type-alias (Tree a) (U (branch a) leaf))
(define-struct: (a) branch ([left : (Tree a)] [item : a] [right : (Tree a)]))
(define-struct: leaf ())
(define: *leaf* : leaf (make-leaf))

(: tree.add (All (a) [(Tree a) a (Comparator a) -> (Tree a)]))
(define (tree.add t e compare)
  (match t
    [(struct branch (left item right))
     (case (compare e item)
       [(less) (make-branch (tree.add left e compare) item right)]
       [(equal) t]
       [(greater) (make-branch left item (tree.add right e compare))])]
    [_ (make-branch *leaf* e *leaf*)]))


(: tree.mem (All (a) [(Tree a) a (Comparator a) -> Boolean]))
(define (tree.mem t e compare)
  (match t
    [(struct branch (left item right))
     (case (compare e item)
       ((less) (tree.mem left e compare))
       ((equal) #t)
       ((greater) (tree.mem right e compare)))]
    [_ #f]))

(: tree.size (All (a) [(Tree a) -> Integer]))
(define (tree.size t)
  (match t
    [(struct branch (left _ right))
     (+ 1 (tree.size left) (tree.size right))]
    [_ 0]))

(: tree.flatten (All (a) [(Tree a) -> (Listof a)]))
(define (tree.flatten t)
  (match t
    [(struct branch (left item right))
     (append (tree.flatten left) (cons item (tree.flatten right)))]
    [_ null]))

(define-struct: (a) Set ([elts : (Tree a)] [compare : (Comparator a)]))

(: create (All (a) [(Comparator a) -> (Set a)]))
(define (create compare) (make-Set *leaf* compare))

(: add (All (a) [(Set a) a -> (Set a)]))
(define (add set e)
  (let ([compare (Set-compare set)])
    (make-Set (tree.add (Set-elts set) e (Set-compare set)) compare)))

(: mem (All (a) [(Set a) a -> Boolean]))
(define (mem a-set e)
  (match a-set
    [(struct Set (tree compare)) (tree.mem tree e compare)]))

(: size (All (a) [(Set a) -> Integer]))
(define (size set) (tree.size (Set-elts set)))

(: of-list (All (a) [(Listof a) (Comparator a) -> (Set a)]))
(define (of-list elts compare)
  (: add* [a (Set a) -> (Set a)])
  (define (add* elt set) (add set elt))
  (let ([#{mt : (Set a)} (create compare)])
    (foldl add* mt elts)))

(: to-list (All (a) [(Set a) -> (Listof a)]))
(define (to-list set) (tree.flatten (Set-elts set)))
