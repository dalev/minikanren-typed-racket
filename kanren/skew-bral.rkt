; binary random access list based on skew numbers
#lang racket/base
(require racket/fixnum)
(provide
  k:size k:lookup k:update k:get-value k:empty
  k:new-var
  var?
  )

  (struct node (val even odd))
  (struct var (name idx))

  ; -- public 

  (define k:empty '(0))

  (define k:size
    (lambda (ls)
      (car ls)))

  (define (k:new-var name ls)
    (let ([x (var name (car ls))])
      (values x (k:associate x ls))))

  (define k:associate
    (lambda (v ls)
      (cons (fx+ 1 (car ls)) (bind v (cdr ls)))))

  (define from-tail
    (lambda (i ls)
      (fx- (fx- (car ls) 1) i)))

  (define k:lookup
    (lambda (v ls)
      (lookup (from-tail (var-idx v) ls) (cdr ls))))

  (define k:update
    (lambda (a-var v ls)
      (cons (car ls) (update (from-tail (var-idx a-var) ls) v (cdr ls)))))

  (define k:get-value
    (lambda (v)
      (cond
        [(node? v) (node-val v)]
        [else (car v)])))

  ; --- helpers
  
  (define (fxzero? x) (fx= x 0))

  (define shift (lambda (n) (fxrshift n 1)))

  ;; CR dalev: horrifying code
  (define bind
    (lambda (v ls)
      (cond
        [(and (pair? ls) (pair? (cdr ls)) (fx= (caar ls) (caadr ls)))
         (cons `(,(fx+ 1 (fx+ (caar ls) (caadr ls)))
                  . ,(node v (cdar ls) (cdadr ls))) (cddr ls))]
        [else (cons `(1 . ,v) ls)])))

  (define lookup-tree
    (lambda (w i t)
      (cond
        [(node? t)
         (if (fxzero? i) t ;(node-val t)
             (let [(w/2 (shift w))]
               (if (fx<= i w/2)
                   (lookup-tree w/2 (fx- i 1) (node-even t))
                   (lookup-tree w/2 (fx- (fx- i 1) w/2) (node-odd t)))))]
        [else (if (fxzero? i) (cons t '()) #f)])))
        ;[else (if (fxzero? i) t (error 'lookup-tree "illegal index"))])))

  (define lookup
    (lambda (i ls)
      (cond
        [(null? ls) #f]
        ;[(null? ls) (error 'k:lookup "illegal index")]
        [else (let [(t (car ls))]
                (if (fx< i (car t))
                    (lookup-tree (car t) i (cdr t))
                    (lookup (fx- i (car t)) (cdr ls))))])))

  (define update-tree
    (lambda (w i v t)
      (cond
        [(node? t)
         (if (fxzero? i) (node v (node-even t) (node-odd t))
             (let [(w/2 (shift w))]
               (if (fx<= i w/2)
                   (node (node-val t)
                              (update-tree w/2 (fx- i 1) v (node-even t))
                              (node-odd t))
                   (node (node-val t)
                              (node-even t)
                              (update-tree w/2 (fx- (fx- i 1) w/2) v (node-odd t))))))]
        [else (if (fxzero? i) v (error 'update-tree "illegal index"))])))

  (define update
    (lambda (i v ls)
      (cond
        [(null? ls) (error 'k:update "illegal index ~s ~s" i v)]
        [else (let [(t (car ls))]
                (if (fx< i (car t))
                    (cons `(,(car t) . ,(update-tree (car t) i v (cdr t))) (cdr ls))
                    (cons t (update (fx- i (car t)) v (cdr ls)))))])))
