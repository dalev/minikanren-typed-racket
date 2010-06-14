#lang scheme/base
(require scheme/match
         scheme/contract)

(provide
  leaf
  add
  mem
  find
  iter
  size
  to-list
  depth)

(define leaf
  (let ()
    (define-struct leaf ())
    (make-leaf)))

(define-struct r-branch (left item right))
(define-struct b-branch (left item right))

(define (color branch)
  (match branch
    [(struct r-branch (_ _ _)) 'red]
    [(struct b-branch (_ _ _)) 'black]))

(define (balance color left item right)
  (match (vector color left item right)
    [(or (vector 'black (struct r-branch [(struct r-branch (a x b)) y c]) z d)
         (vector 'black (struct r-branch [a x (struct r-branch (b y c))]) z d)
         (vector 'black a x (struct r-branch [(struct r-branch (b y c)) z d]))
         (vector 'black a x (struct r-branch [b y (struct r-branch (c z d))])))
     (make-r-branch (make-b-branch a x b) y (make-b-branch c z d))]
    [else
      (case color
        [(red)   (make-r-branch left item right)]
        [(black) (make-b-branch left item right)])]))

(define (darken-root t)
  (match t
    [(struct r-branch (left item right)) (make-b-branch left item right)]
    [(struct b-branch (_ _ _)) t]))

(define (add t item compare)
  (define (insert-item t)
    (match t
      [(or (struct r-branch (left item* right))
           (struct b-branch (left item* right)))
       (case (compare item item*)
         [(less)    (balance (color t) (insert-item left) item* right)]
         [(greater) (balance (color t) left item* (insert-item right))]
         [(equal) t])]
      [else (make-r-branch leaf item leaf)]))
  (darken-root (insert-item t)))

(define (find t e compare)
  (match t
    [(or (struct r-branch (left item right)) (struct b-branch (left item right)))
     (case (compare e item)
       ((less) (find left e compare))
       ((equal) (vector item))
       ((greater) (find right e compare)))]
    [else #f]))

(define (mem t e compare) (vector? (find t e compare)))

(define (iter t f)
  (match t
    [(or (struct r-branch (left item right)) (struct b-branch (left item right)))
     (f item)
     (iter left f)
     (iter right f)]
    [else (void)]))

(define (size t)
  (match t
    [(or (struct r-branch (left _ right)) (struct b-branch (left _ right)))
     (+ 1 (size left) (size right))]
    [else 0]))

(define (to-list t)
  (match t
    [(or (struct r-branch (left item right)) (struct b-branch (left item right)))
     (append (to-list left) (cons item (to-list right)))]
    [else '()]))

(define (depth t)
  (match t
    [(or (struct r-branch (left _ right)) (struct b-branch (left _ right)))
     (add1 (max (depth left) (depth right)))]
    [else 0]))
