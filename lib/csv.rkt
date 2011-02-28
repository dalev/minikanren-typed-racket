#lang racket
(require (planet jaymccarthy/sqlite)
         (planet dalev/core/lib/string)
         racket/generator)

(provide/contract
  [in-raw-csv ((input-port?) (#:reverse? boolean? #:separator char?) . ->* . sequence?)])

(define (record-separator? c) (char=? c #\newline))

;; CR dalev: implement a nicer CSV interface that represents records as maps,
;;           allow a header to be supplied / used as override

;; CR dalev: skip blank lines
;; CR dalev: don't overrun field-buffer
(define (in-raw-csv in #:separator [separator #\,] #:reverse? [reverse? false]) 

  (define rev (if reverse? values reverse))

  (define field-buffer (make-string 1024 #\space))

  (define (buffer! buf-idx c) 
    (string-set! field-buffer buf-idx c))

  (define (emit-field! buf-idx)
    (substring field-buffer 0 buf-idx))

  (define (yield! csv-fields buf-idx)
    (yield (rev (cons (emit-field! buf-idx) csv-fields)))
    (values 'start-field '() 0))

  (in-generator
    (for/fold ([state 'start-field]
               [csv-fields '()]
               [buf-idx 0])
              ([c (in-input-port-chars in)])
      (case state
        [(in-field) 
         (cond [(char=? c separator) 
                (values 'start-field (cons (emit-field! buf-idx) csv-fields) 0)]
               [(record-separator? c) 
                (yield! csv-fields buf-idx)]
               [else (buffer! buf-idx c)
                     (values 'in-field csv-fields (add1 buf-idx))])]
        [(start-field) 
         (cond [(char=? separator c) 
                (values 'start-field (cons (emit-field! buf-idx) csv-fields) 0)]
               [(char=? #\" c)
                (values 'in-quoted-field csv-fields buf-idx)]
               [(record-separator? c) 
                (yield! csv-fields buf-idx)]
               [else 
                 (buffer! buf-idx c)
                 (values 'in-field csv-fields (add1 buf-idx))])]
        [(in-quoted-field) 
         (cond [(char=? c #\")
                (values 'in-quoted-field-after-quote csv-fields buf-idx)]
               [else (buffer! buf-idx c)
                     (values 'in-quoted-field csv-fields (add1 buf-idx))])]
        [(in-quoted-field-after-quote) 
         (cond [(char=? c #\")
                (buffer! buf-idx c)
                (values 'in-quoted-field csv-fields (add1 buf-idx))]
               [(char=? separator c)
                (values 'start-field (cons (emit-field! buf-idx) csv-fields) 0)]
               [(record-separator? c) 
                (yield! csv-fields buf-idx)]
               [(raise (exn:fail "Expected double-quote or separator"
                                 (current-continuation-marks)))])]))))
                
        

