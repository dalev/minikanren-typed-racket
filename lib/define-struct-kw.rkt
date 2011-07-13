
#lang racket
(require (for-syntax unstable/syntax)
         (for-syntax syntax/struct)
         (for-syntax racket/match))

(provide define-struct/kw)

(define-for-syntax (identity x) x)

(define-for-syntax (symbol-append s0 s1 . ss)
  (string->symbol
   (apply string-append (symbol->string s0) (symbol->string s1) (map symbol->string ss))))

(define-for-syntax (symbol->keyword s) (string->keyword (symbol->string s)))

(define-for-syntax (identifier->keyword id) (symbol->keyword (syntax-e id)))

(define-for-syntax (interleave xs ys)
  (match (cons xs ys)
    [(cons (cons x xs*) (cons y ys*))
     (cons x (cons y (interleave xs* ys*)))]
    [(cons '() '()) '()]
    ;; CR dalev: bogus use of raise-type-error
    [else (raise-type-error 'interleave "lists of the same length" 0 xs ys)]))

(define-for-syntax (not-identifier? x) (not (identifier? x)))

(define-syntax (define-struct/kw stx)
  (syntax-case stx ()
    [(_ name (field ...))
     (let* ([field-list (syntax-case stx () [(_ name field-list) #'field-list])]
            [fields (syntax->list field-list)])
       (begin
         (unless (identifier? #'name) 
           (raise-syntax-error #f "Expected a structure name" #'name))
         (match (filter not-identifier? fields)
           ['() (void)]
           [non-ids
            (raise-syntax-error #f "Expected field names" stx field-list non-ids)])
         (match (check-duplicate-identifier fields)
           [#f (void)]
           [id (raise-syntax-error #f "Found duplicate field name" stx id)])
         
         (with-syntax ([create-name 
                        (datum->syntax #'name 
                                       (symbol-append 'create- (syntax-e #'name)))]
                       [(kw ...) (map identifier->keyword fields)]
                       [(field-idx ...) (build-list (length fields) identity)]
                       [(struct: _ predicate selector ...)
                        (build-struct-names #'name 
                                            fields 
                                            #f
                                            #t
                                            #'name)])
           ;; CR dalev: Is shoving the (make-inspector) in there the right choice?
           ;; CR dalev: Can we used define-struct/derived to promote reuse?
           ;;           Maybe shove it into a let to hide the default make-name?
           #`(begin 
               (define-values (struct: make-name predicate struct-ref struct-set!)
                 (make-struct-type (quote name) #f #,(length fields) 0
                                   #f null (make-inspector)))
               (define-syntax name
                 #,(build-struct-expand-info #'name fields #f #t #f null null))
               (define selector 
                 (make-struct-field-accessor struct-ref field-idx (quote field)))
               ...
               (define (create-name #,@(interleave (syntax->list #'(kw ...)) fields))
                 (make-name field ...))))))
       ]))

;; CR dalev: factor these tests out
(define-struct/kw fish (color weight dead?))

(let ([f (create-fish #:dead? #f #:weight 5 #:color 'blue)]
      [g (create-fish #:color 'blue #:dead? #f #:weight 5)])
  (and (= (fish-weight f) (fish-weight g))
       (eq? (fish-color f) (fish-color g))
       (eq? (fish-dead? f) (fish-dead? g))))
#;
(match (create-fish #:dead? #t #:weight 7 #:color 'red)
  [(fish a b c) (list c b a)])

#;
(equal? (match (create-fish #:dead? #t #:weight 7 #:color 'red)
          [(struct* fish ([weight a] [dead? b] [color c])) (list a b c)])
        (list 7 #t 'red))
