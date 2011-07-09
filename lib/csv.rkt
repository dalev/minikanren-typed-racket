#lang racket
(require racket/generator
         racket/unsafe/ops)

(provide/contract
  [in-records
    ((input-port?) (#:separator char? #:buffer-size fixnum?) 
     . ->* . 
     sequence?)]
  [in-raw
    ((input-port?) (#:reverse? boolean? #:separator char? #:buffer-size fixnum?) 
     . ->* . 
     sequence?)]
  [in-raw-chunks 
    ((input-port?) (#:reverse? boolean? #:separator char? #:buffer-size fixnum?) 
     . ->* . 
     sequence?)])

(define byte-double-quote (char->integer #\"))
(define (double-quote? byte) (eq? byte byte-double-quote))

(define *byte-space* (char->integer #\space))

(define *default-buffer-size* (* 8 1024))
(define *default-separator* #\,)

(define (in-records in 
                    #:separator [separator *default-separator*]
                    #:buffer-size [buffer-size *default-buffer-size*])
  (define *header* #f)
  (in-generator
    (for ([chunk (in-raw-chunks in 
                                #:separator separator
                                #:buffer-size buffer-size)])
      (for ([row (in-list chunk)])
        (if *header*
          (yield 
            (for/hash ([key (in-list *header*)]
                       [val (in-list row)])
              (values key val)))
          (set! *header* row))))))

(define (in-raw in
                #:separator [separator *default-separator*] 
                #:reverse? [reverse? false]
                #:buffer-size [buffer-size *default-buffer-size*])
  (in-generator
    (for* ([records (in-raw-chunks in 
                                   #:separator separator
                                   #:reverse? reverse?
                                   #:buffer-size buffer-size)]
           [record (in-list records)])
          (yield record))))

(define (in-raw-chunks in 
                       #:separator [separator *default-separator*] 
                       #:reverse? [reverse? false]
                       #:buffer-size [buffer-size *default-buffer-size*]) 

  (define rev (if reverse? values reverse))

  (define field-buffer (make-bytes 256))

  (define (double-field-buffer!)
    (let* ([current-length (bytes-length field-buffer)]
           [new-buffer (make-bytes (* 2 current-length))])
      (bytes-copy! new-buffer 0 field-buffer)
      (set! field-buffer new-buffer)))

  (define (buffer! buf-idx c) 
    (when (>= buf-idx (unsafe-bytes-length field-buffer))
      (double-field-buffer!))
    (unsafe-bytes-set! field-buffer buf-idx c))

  (define (emit-field! buf-idx)
    (bytes->string/locale (subbytes field-buffer 0 buf-idx)))

  (define (field-buffer-blank? #:length len)
    (for/and ([byte (in-bytes field-buffer 0 len)])
      (= byte *byte-space*)))

  (define (yield! csv-fields buf-idx rows)
    (values 'start-field '() 0
            (if (and (null? csv-fields) (field-buffer-blank? #:length buf-idx))
              '()
              (cons (rev (cons (emit-field! buf-idx) csv-fields))
                    rows))))

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
