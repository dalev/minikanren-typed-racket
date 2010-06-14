#lang scheme
(require (for-syntax scheme/match))

(define-for-syntax (not-keyword-syntax? stx)
  (not (keyword? (syntax-e stx))))

(define-for-syntax (keyword-stx->symbol kw)
  (string->symbol (keyword->string (syntax-e kw))))

(define-for-syntax (interleave xs ys)
  (match (cons xs ys)
    [(cons (cons x xs*) (cons y ys*))
     (cons x (cons y (interleave xs* ys*)))]
    [(cons '() '()) '()]))

(define-syntax (def stx)
  (syntax-case stx ()
    [(_ (stuff ...))
     (raise-syntax-error #f "Need a body expression" stx)]
    [(_ () body0 body ...)
     (raise-syntax-error #f "Need a procedure name" stx)]
    [(_ (proc kw ...) body ...)
     (let* ([kws (syntax->list #'(kw ...))]
            [syms (map (lambda (k) (datum->syntax k (keyword-stx->symbol k))) kws)])
       (match (filter not-keyword-syntax? kws)
         ['() (void)]
         [(cons kw _kws)
          (raise-syntax-error #f "Expected a keyword" stx kw)])
       #`(begin
           (define (proc #,@(interleave kws syms)) body ...)))]))

(def (f #:x #:y) (+ x y 1))