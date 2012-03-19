#lang racket/base
(require racket/match
         racket/contract
         (for-syntax racket/base racket/match syntax/parse racket/syntax))

(provide
  [rename-out (*empty* empty)]
  empty?
  add
  remove
  mem
  find
  iter
  size
  to-list
  depth
  
  invariant?)

(define double-red 'red)
(define red 'red)
(define black 'black)
(define double-black 'double-black)

(define-syntax (colorful-structs stx)
  (syntax-parse stx
    [(_colorful-structs name:id (color:id ...) (field:id ...))
     (define colors (syntax->list #'(color ...)))
     (define name-sym (syntax-e #'name))
     (define ascending (list #'double-red #'red #'black #'double-black))
     (define (color->constructor c)
       (format-id #'name "~a-~a" (syntax-e c) name-sym))
     (define (following color-id ordered-colors)
       (define runtime-error-proc
         #'(λ (field ...) (error 'successor "~a" (quote color-id))))
       (match (memf (λ (v) (free-identifier=? v color-id)) ordered-colors)
         [(cons _ (cons succ _)) 
          (if (for/or ([c (in-list colors)]) (free-identifier=? c succ))
            (color->constructor succ)
            runtime-error-proc)]
         [(list _) runtime-error-proc]
         [(list) (raise-syntax-error #f "No such color" stx color-id)]
         [#f (raise-syntax-error #f "No such color (#f)" stx color-id)]))
     (define (successor color) (following color ascending))
     (define (predecessor color) (following color (reverse ascending)))
     (with-syntax ([(colorful-name ...)
                    (for/list ([c (in-list colors)])
                      (color->constructor c))]
                   [(colorful-name? ...)
                    (for/list ([c (in-list colors)])
                      (format-id #'name "~a-~a?" (syntax-e c) name-sym))]
                   [(colorful-name/black+1 ...)
                    (for/list ([c (in-list colors)])
                      (successor c))]
                   [(colorful-name/black-1 ...)
                    (for/list ([c (in-list colors)])
                      (predecessor c))]
                   [any-name (format-id #'name "any-~a" name-sym)]
                   [name->color (format-id #'name "~a->color" name-sym)]
                   [name-black+1 (format-id #'name "~a-black+1" name-sym)]
                   [name-black-1 (format-id #'name "~a-black-1" name-sym)])
       #'(begin (struct colorful-name (field ...)) 
                ...
                (define (name->color t)
                  (cond [(colorful-name? t) color] 
                        ...))
                (define (name-black+1 t)
                  (match t
                    [(colorful-name field ...) 
                     (colorful-name/black+1 field ...)]
                    ...))
                (define (name-black-1 t)
                  (match t
                    [(colorful-name field ...) 
                     (colorful-name/black-1 field ...)]
                    ...))
                (define-match-expander any-name
                  (λ (stx)
                    (syntax-parse stx
                      [(_ field ...)
                       #'(or (colorful-name field ...) ...)])))
                ))]))

;; We encode the color in the structure types to save a word per node.
;; We also represent leaf nodes explicitly rather than as 
;;   (branch empty item empty) 
;; to save two words on each node at the fringe of the tree.
(colorful-structs leaf (double-red red black double-black) (item))
(colorful-structs branch (double-red red black double-black) (left item right))
(colorful-structs empty (black double-black) ())

;; CR dalev: the following three functions match on t twice each
(define (color t)
  (match t
    [(any-branch _ _ _) (branch->color t)]
    [(any-leaf _) (leaf->color t)]
    [(any-empty) (empty->color t)]))

(define (black+1 t)
  (match t
    ['double-red red]
    ['red black]
    ['black double-black]
    [(any-branch _ _ _) (branch-black+1 t)]
    [(any-leaf _) (leaf-black+1 t)]
    [(any-empty) (empty-black+1 t)]))

(define (black-1 t)
  (match t
    ['double-black black]
    ['black red]
    ['red double-red]
    [(any-branch _ _ _) (branch-black-1 t)]
    [(any-leaf _) (leaf-black-1 t)]
    [(any-empty) (empty-black-1 t)]))

(define (double-black? t) (eq? (color t) double-black))

(define empty? black-empty?)
(define *empty* (black-empty))
(define *bb-empty* (double-black-empty))

(define (red? t) (eq? (color t) red))
(define (black? t) (eq? (color t) black))
(define (implies a b) (if a b #t))
;; Immediate subtrees of each branch shouldn't both be empty
;; No intermediate (double) colors should leak out
;; Children of red branches are black
(define (invariant? t)
  (and (or (red? t) (black? t))
       (match t
         [(any-branch left _ right)
          (if (and (empty? left) (empty? right))
            #f
            (and (implies (red? t) (and (black? left) (black? right)))
                 (invariant? left) 
                 (invariant? right)))]
         [_ #t])))

;; (black-node _ _ _) <> empty
(define (black-node left item right)
  (if (and (empty? left) (empty? right))
    (black-leaf item)
    (black-branch left item right)))

(define (balance color left item right)
  (when (and (empty? left) (empty? right))
    (error 'balance "At least one of left or right must be nonempty"))
  (case color
    [(black double-black)
     (match (vector left item right)
       [(or (vector (red-branch (red-branch a x b) y c) z d)
            (vector (red-branch a x (red-branch b y c)) z d)
            (vector a x (red-branch (red-branch b y c) z d))
            (vector a x (red-branch b y (red-branch c z d))))
        ((if (eq? color 'black) ;; black-1
           red-branch
           black-branch)
         (black-node a x b) y (black-node c z d))]
       [(vector (red-branch (red-leaf x) y c) z d)
        ;; a = b = empty
        (red-branch (black-leaf x) y (black-node c z d))]
       [(or (vector (red-branch a x (red-leaf y)) z d)
            (vector a x (red-branch (red-leaf y) z d)))
        ;; b = c = empty
        (red-branch (black-node a x *empty*) y (black-node *empty* z d))]
       [(vector a x (red-branch b y (red-leaf z)))
        ;; c = d = empty
        (red-branch (black-node a x b) y (black-leaf z))]
       [_ (if (eq? color double-black)
            (error 'balance "To do")
            (black-branch left item right))])]
    [(red) (red-branch left item right)]))

(define (darken-root t)
  (match t
    [(red-branch left item right) (black-branch left item right)]
    [(red-leaf item) (black-leaf item)]
    [(or (black-leaf _) (black-branch _ _ _)) t]))

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
         [(less)    (balance (color t) (red-leaf item) item* *empty*)]
         [(greater) (balance (color t) *empty* item* (red-leaf item))]
         [(equal) t])]
                             
      [(black-empty) (red-leaf item)]))
  (darken-root (insert-item t)))

(define (node color left item right)
  (if (and (empty? left) (empty? right))
    (case color
      [(red) (red-leaf item)]
      [(black) (black-leaf item)])
    (case color
      [(red) (red-branch left item right)]
      [(black) (black-branch left item right)])))

(define (yank-max t)
  (match t
    [(any-branch left item (black-empty)) (values left item)]
    [(any-branch left item right) 
     (define-values (new-right max-item) (yank-max right))
     (values (node (color t) left item new-right)
             max-item)]
    [(any-leaf _) (values *empty* t)]
    [(black-empty) (error 'yank-max "invoked on empty")]))

(define (remove t e compare)
  (darken-root (rm t e compare)))

(define (rm t e compare)
  (match t
    [(any-leaf item)
     (case (compare e item)
       [(equal) (delete! t)]
       [else t])]
    [(any-branch left item right) 
     (case (compare e item)
       [(less)    (bubble (color t) (rm left e compare) item right)]
       [(greater) (bubble (color t) left item (rm right e compare))]
       [(equal) (delete! t)])]
    [(any-empty) t]))

(define (delete! t)
  (match t
    [(red-leaf _) *empty*]
    [(black-leaf _) *bb-empty*]

    [(or (black-branch (red-leaf r-item) _ (black-empty))
         (black-branch (black-empty) _ (red-leaf r-item)))
     (black-leaf r-item)]
    [(or (black-branch (black-empty) _ (red-branch left item right))
         (black-branch (red-branch left item right) _ (black-empty)))
     (black-branch left item right)]
    
    [(any-branch left _ right)
     (define-values (new-left left-max) (yank-max left))
     ;; CR dalev: Might uses `bubble' instead of `node', but
     ;; I'm fairly confident that neither new-left nor right
     ;; can be double-black.
     (node (color t) new-left left-max right)]))

(define (bubble color left item right)
  (if (or (double-black? left) (double-black? right))
    (balance (black+1 color) (black-1 left) item (black-1 right))
    (node color left item right)))

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
    [(black-empty) #f]))

(define (mem t e compare) (vector? (find t e compare)))

(define (iter t f)
  (match t
    [(any-branch left item right)
     (f item)
     (iter left f)
     (iter right f)]
    [(any-leaf item) (f item)]
    [(black-empty) (void)]))

(define (size t)
  (match t
    [(any-branch left _ right)
     (+ 1 (size left) (size right))]
    [(any-leaf _) 1]
    [(black-empty) 0]))

(define (to-list t)
  (match t
    [(any-branch left item right)
     (append (to-list left) (cons item (to-list right)))]
    [(any-leaf item) (list item)]
    [(black-empty) '()]))

(define (depth t)
  (match t
    [(any-branch left _ right) (add1 (max (depth left) (depth right)))]
    [(any-leaf _) 1]
    [(black-empty) 0]))

(define (internal-equal? x y)
  (and (eq? (color x) (color y))
       (match* (x y)
         [((any-branch left item right) (any-branch left* item* right*))
          (and (equal? item item*)
               (internal-equal? left left*)
               (internal-equal? right right*))]
         [((any-leaf item) (any-leaf item*))
          (equal? item item*)]
         [((any-empty) (any-empty)) #t] ;; color already checked
         [(_ _) #f])))

(require rackunit)
(provide tests)
(define tests
  (test-suite 
    "Internal red-black-tree tests"

    (check internal-equal? (black-1 (red-leaf 42)) (double-red-leaf 42)))
  )
