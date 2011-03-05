#lang racket
(require (planet jaymccarthy/sqlite)
         racket/generator)

(provide/contract
  [in-raw-csv ((input-port?) (#:reverse? boolean? #:separator char?) . ->* . sequence?)])

(define byte-double-quote (char->integer #\"))
(define (double-quote? byte) (eq? byte byte-double-quote))

;; CR dalev: implement a nicer CSV interface that represents records as maps,
;;           allow a header to be supplied / used as override

;; CR dalev: skip blank lines
;; CR dalev: don't overrun field-buffer
(define (in-raw-csv in #:separator [separator #\,] #:reverse? [reverse? false]) 

  (define rev (if reverse? values reverse))

  (define field-buffer (make-bytes 1024))

  (define (buffer! buf-idx c) 
    (bytes-set! field-buffer buf-idx c))

  (define (emit-field! buf-idx)
    (bytes->string/locale (subbytes field-buffer 0 buf-idx)))

  (define (yield! csv-fields buf-idx)
    (yield (rev (cons (emit-field! buf-idx) csv-fields)))
    (values 'start-field '() 0))

  (define input-bytes (make-bytes (* 4 1024)))

  (define byte-field-separator (char->integer separator))
  (define byte-record-separator (char->integer #\newline))

  (define (field-sep? byte) (eq? byte byte-field-separator))
  (define (record-sep? byte) (eq? byte byte-record-separator))

  (in-generator
    (let loop ([state 'start-field]
               [csv-fields '()]
               [buf-idx 0])
      (define avail (read-bytes-avail! input-bytes in))
      (cond
        [(number? avail) 
         (define-values (new-state new-csv-fields new-buf-idx)
           (for/fold ([state state] [csv-fields csv-fields] [buf-idx buf-idx])
             ([i (in-range 0 avail)]
              [c (in-bytes input-bytes)])
             (case state
               [(in-field) 
                (cond [(field-sep? c) 
                       (values 'start-field (cons (emit-field! buf-idx) csv-fields) 0)]
                      [(record-sep? c) 
                       (yield! csv-fields buf-idx)]
                      [else (buffer! buf-idx c)
                            (values 'in-field csv-fields (add1 buf-idx))])]
               [(start-field) 
                (cond [(field-sep? c) 
                       (values 'start-field (cons (emit-field! buf-idx) csv-fields) 0)]
                      [(double-quote? c)
                       (values 'in-quoted-field csv-fields buf-idx)]
                      [(record-sep? c) 
                       (yield! csv-fields buf-idx)]
                      [else 
                        (buffer! buf-idx c)
                        (values 'in-field csv-fields (add1 buf-idx))])]
               [(in-quoted-field) 
                (cond [(double-quote? c)
                       (values 'in-quoted-field-after-quote csv-fields buf-idx)]
                      [else (buffer! buf-idx c)
                            (values 'in-quoted-field csv-fields (add1 buf-idx))])]
               [(in-quoted-field-after-quote) 
                (cond [(double-quote? c)
                       (buffer! buf-idx c)
                       (values 'in-quoted-field csv-fields (add1 buf-idx))]
                      [(field-sep? c)
                       (values 'start-field (cons (emit-field! buf-idx) csv-fields) 0)]
                      [(record-sep? c) 
                       (yield! csv-fields buf-idx)]
                      [(raise (exn:fail "Expected double-quote or separator"
                                        (current-continuation-marks)))])])))
         (loop new-state new-csv-fields new-buf-idx)]
        [(eof-object? avail) (void)]
        [(procedure? avail) (error 'in-csv "read-bytes-avail! returned a procedure")]))))
