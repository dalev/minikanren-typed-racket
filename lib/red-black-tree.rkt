#lang racket/base
(require racket/match
         racket/contract)

(provide
  empty
  add
  mem
  find
  iter
  size
  to-list
  depth)

(define empty
  (let ()
    (struct empty ())
    (empty)))

(struct r-leaf (item))
(struct b-leaf (item))

(struct r-branch (left item right))
(struct b-branch (left item right))

(define (color branch)
  (match branch
    [(r-branch _ _ _) 'red]
    [(b-branch _ _ _) 'black]))

(define (balance color left item right)
  (match (vector color left item right)
    [(or (vector 'black (r-branch (r-branch a x b) y c) z d)
         (vector 'black (r-branch a x (r-branch b y c)) z d)
         (vector 'black a x (r-branch (r-branch b y c) z d))
         (vector 'black a x (r-branch b y (r-branch c z d))))
     (r-branch (b-branch a x b) y (b-branch c z d))]
    [else
      (case color
        [(red)   (r-branch left item right)]
        [(black) (b-branch left item right)])]))

(define (darken-root t)
  (match t
    [(r-branch left item right) (b-branch left item right)]
    [(b-branch _ _ _) t]))

(define (add t item compare)
  (define (insert-item t)
    (match t
      [(or (r-branch left item* right)
           (b-branch left item* right))
       (case (compare item item*)
         [(less)    (balance (color t) (insert-item left) item* right)]
         [(greater) (balance (color t) left item* (insert-item right))]
         [(equal) t])]
      [else (r-branch empty item empty)]))
  (darken-root (insert-item t)))

(define (find t e compare)
  (match t
    [(or (r-branch left item right) (b-branch left item right))
     (case (compare e item)
       ((less) (find left e compare))
       ((equal) (vector item))
       ((greater) (find right e compare)))]
    [else #f]))

(define (mem t e compare) (vector? (find t e compare)))

(define (iter t f)
  (match t
    [(or (r-branch left item right) (b-branch left item right))
     (f item)
     (iter left f)
     (iter right f)]
    [else (void)]))

(define (size t)
  (match t
    [(or (r-branch left _ right) (b-branch left _ right))
     (+ 1 (size left) (size right))]
    [else 0]))

(define (to-list t)
  (match t
    [(or (r-branch left item right) (b-branch left item right))
     (append (to-list left) (cons item (to-list right)))]
    [else '()]))

(define (depth t)
  (match t
    [(or (r-branch left _ right) (b-branch left _ right))
     (add1 (max (depth left) (depth right)))]
    [else 0]))
