#lang racket/base
(require racket/match
         racket/contract
         (prefix-in tree: "red-black-tree.rkt"))

;; CR dalev: add contracts
(provide
  create
  add
  find
  of-alist
  to-alist)

(define-struct map (tree compare-pair-by-key))

(define (create compare-key) 
  (make-map tree:empty (lambda (p q) (compare-key (car p) (car q)))))

(define (add m #:key key #:data data)
  (match m
    [(struct map (tree compare))
     (make-map (tree:add tree (cons key data) compare) compare)]))

(define (find m key)
  (match m
    [(struct map (tree compare))
     (let ([key-pair (cons key 'bogus)])
       (match (tree:find tree key-pair compare)
         [(vector binding) binding] ;; CR dalev: yuck.
         [other other]))]))

(define (of-alist alist #:compare compare-key)
  (define (compare p q) (compare-key (car p) (car q)))
  (make-map
    (for/fold ([tree tree:empty]) ([e (in-list alist)])
      (tree:add tree e compare))
    compare))

(define (to-alist m)
  (match m
    [(struct map (tree _))
     (tree:to-list tree)]))
