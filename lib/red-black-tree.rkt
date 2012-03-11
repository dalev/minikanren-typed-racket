#lang racket/base
(require racket/match
         (for-syntax racket/base)
         racket/contract)

(provide
  empty
  add
  mem
  find
  iter
  size
  to-list
  depth
  
  invariant?)

(define-values (empty empty?)
  (let ()
    (struct empty ())
    (values (empty) empty?)))

;; We encode the color in the structure types to save a word per node.
;; We also represent leaf nodes explicitly rather than as 
;;   (branch empty item empty) 
;; to save two words on each node at the fringe of the tree.
(struct r-leaf (item))
(struct b-leaf (item))

(struct r-branch (left item right))
(struct b-branch (left item right))

;; Most tree procedures other than add & balance don't care about the node
;; color, and so we extend the pattern language to capture this common case.
(define-match-expander any-branch
  (λ (stx)
    (syntax-case stx ()
      [(_ left item right) #'(or (r-branch left item right)
                                 (b-branch left item right))])))

(define-match-expander any-leaf
  (λ (stx)
    (syntax-case stx ()
      [(_ item) #'(or (b-leaf item) (r-leaf item))])))

;; Immediate subtrees of each branch shouldn't both be empty
(define (invariant? t)
  (match t
    [(any-branch left _ right)
     (if (and (empty? left) (empty? right))
       #f
       (and (invariant? left) (invariant? right)))]
    [_ #t]))

;; (black-node _ _ _) <> empty
(define (black-node left item right)
  (if (and (empty? left) (empty? right))
    (b-leaf item)
    (b-branch left item right)))

(define (color branch)
  (match branch
    [(or (r-leaf _) (r-branch _ _ _)) 'red]
    [(or (b-leaf _) (b-branch _ _ _)) 'black]))

(define (balance color left item right)
  (when (and (empty? left) (empty? right))
    (error 'balance "At least one of left or right must be nonempty"))
  (case color
    [(black)
     (match (vector left item right)
       [(or (vector (r-branch (r-branch a x b) y c) z d)
            (vector (r-branch a x (r-branch b y c)) z d)
            (vector a x (r-branch (r-branch b y c) z d))
            (vector a x (r-branch b y (r-branch c z d))))
        (r-branch (black-node a x b) y (black-node c z d))]
       [(vector (r-branch (r-leaf x) y c) z d)
        ;; a = b = empty
        (r-branch (b-leaf x) y (black-node c z d))]
       [(or (vector (r-branch a x (r-leaf y)) z d)
            (vector a x (r-branch (r-leaf y) z d)))
        ;; b = c = empty
        (r-branch (black-node a x empty) y (black-node empty z d))]
       [(vector a x (r-branch b y (r-leaf z)))
        ;; c = d = empty
        (r-branch (black-node a x b) y (b-leaf z))]
       [_ (b-branch left item right)])]
    [(red) (r-branch left item right)]))

(define (darken-root t)
  (match t
    [(r-branch left item right) (b-branch left item right)]
    [(r-leaf item) (b-leaf item)]
    [(or (b-leaf _) (b-branch _ _ _)) t]))

(define (add t item compare)
  (define (insert-item t)
    (match t
      [(any-branch left item* right)
       (case (compare item item*)
         [(less)    (balance (color t) (insert-item left) item* right)]
         [(greater) (balance (color t) left item* (insert-item right))]
         [(equal) t])]
      [(any-leaf item*)
       (case (compare item item*)
         [(less)    (balance (color t) (r-leaf item) item* empty)]
         [(greater) (balance (color t) empty item* (r-leaf item))]
         [(equal) t])]
                             
      [_ (r-leaf item)]))
  (darken-root (insert-item t)))

(define (find t e compare)
  (match t
    [(any-branch left item right)
     (case (compare e item)
       ((less) (find left e compare))
       ((equal) (vector item))
       ((greater) (find right e compare)))]
    [(any-leaf item)
     (if (eq? (compare e item) 'equal)
       (vector item)
       #f)]
    [_ #f]))

(define (mem t e compare) (vector? (find t e compare)))

(define (iter t f)
  (match t
    [(any-branch left item right)
     (f item)
     (iter left f)
     (iter right f)]
    [(any-leaf item) (f item)]
    [_ (void)]))

(define (size t)
  (match t
    [(any-branch left _ right)
     (+ 1 (size left) (size right))]
    [(any-leaf _) 1]
    [_ 0]))

(define (to-list t)
  (match t
    [(any-branch left item right)
     (append (to-list left) (cons item (to-list right)))]
    [(any-leaf item) (list item)]
    [_ '()]))

(define (depth t)
  (match t
    [(any-branch left _ right) (add1 (max (depth left) (depth right)))]
    [(any-leaf _) 1]
    [_ 0]))
