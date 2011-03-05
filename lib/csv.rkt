#lang racket
(require (planet jaymccarthy/sqlite)
         racket/generator
         racket/unsafe/ops)

(provide/contract
  [in-raw-csv-chunks 
    ((input-port?) (#:reverse? boolean? #:separator char? #:buffer-size fixnum?) 
     . ->* . 
     sequence?)])

(define byte-double-quote (char->integer #\"))
(define (double-quote? byte) (eq? byte byte-double-quote))

;; CR dalev: implement a nicer CSV interface that represents records as maps,
;;           allow a header to be supplied / used as override

;; CR dalev: skip blank lines
;; CR dalev: don't overrun field-buffer
(define (in-raw-csv-chunks in 
                           #:separator [separator #\,] 
                           #:reverse? [reverse? false]
                           #:buffer-size [buffer-size (* 8 1024)]) 

  (define rev (if reverse? values reverse))

  (define field-buffer (make-bytes 1024))

  (define (buffer! buf-idx c) 
    (when (>= buf-idx (unsafe-bytes-length field-buffer))
      (error 'buffer! "need to resize field-buffer"))
    (unsafe-bytes-set! field-buffer buf-idx c))

  (define (emit-field! buf-idx)
    (bytes->string/locale (subbytes field-buffer 0 buf-idx)))

  (define (yield! csv-fields buf-idx rows)
    (values 'start-field '() 0
            (cons (rev (cons (emit-field! buf-idx) csv-fields))
                  rows)))

  (define input-bytes (make-bytes buffer-size))

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
         (define-values (new-state new-csv-fields new-buf-idx rows)
           (for/fold ([state state] [csv-fields csv-fields] [buf-idx buf-idx] [rows '()])
             ([i (in-range 0 avail)]
              [c (in-bytes input-bytes)])
             (case state
               [(in-field) 
                (cond [(field-sep? c) 
                       (values 'start-field (cons (emit-field! buf-idx) csv-fields) 0 
                               rows)]
                      [(record-sep? c) 
                       (yield! csv-fields buf-idx rows)]
                      [else (buffer! buf-idx c)
                            (values 'in-field csv-fields (add1 buf-idx) rows)])]
               [(start-field) 
                (cond [(field-sep? c) 
                       (values 'start-field (cons (emit-field! buf-idx) csv-fields) 0
                               rows)]
                      [(double-quote? c)
                       (values 'in-quoted-field csv-fields buf-idx rows)]
                      [(record-sep? c) 
                       (yield! csv-fields buf-idx rows)]
                      [else 
                        (buffer! buf-idx c)
                        (values 'in-field csv-fields (add1 buf-idx) rows)])]
               [(in-quoted-field) 
                (cond [(double-quote? c)
                       (values 'in-quoted-field-after-quote csv-fields buf-idx rows)]
                      [else (buffer! buf-idx c)
                            (values 'in-quoted-field csv-fields (add1 buf-idx) rows)])]
               [(in-quoted-field-after-quote) 
                (cond [(double-quote? c)
                       (buffer! buf-idx c)
                       (values 'in-quoted-field csv-fields (add1 buf-idx) rows)]
                      [(field-sep? c)
                       (values 'start-field (cons (emit-field! buf-idx) csv-fields) 0 
                               rows)]
                      [(record-sep? c) 
                       (yield! csv-fields buf-idx rows)]
                      [(raise (exn:fail "Expected double-quote or separator"
                                        (current-continuation-marks)))])])))
         (yield (reverse rows))
         (loop new-state new-csv-fields new-buf-idx)]
        [(eof-object? avail) (void)]
        [(procedure? avail) (error 'in-csv "read-bytes-avail! returned a procedure")]))))
