#lang racket
(require (planet jaymccarthy/sqlite)
         (planet dalev/core/lib/string)
         racket/generator)

(provide/contract
  [in-raw-csv ((input-port?) (#:reverse? boolean? #:separator char?) . ->* . sequence?)])

(define (record-separator? c) (char=? c #\newline))

;; CR dalev: skip blank lines
;; CR dalev: don't overrun field-buffer
(define (in-raw-csv in #:separator [separator #\,] #:reverse? [reverse? false]) 

  (define rev (if reverse? values reverse))

  (define field-buffer (make-string 1024 #\space))
  (define buffer-position 0)
  (define (buffer! c) 
    ;; Could use unsafe op if the loop took care of counting the characters
    ;; and resizing the buffer
    (string-set! field-buffer buffer-position c)
    (set! buffer-position (add1 buffer-position)))
  (define (buffer-clear!) (set! buffer-position 0))
  (define (buffer->string) (substring field-buffer 0 buffer-position))

  (define (emit-field!)
    (let ([x (buffer->string)])
      (buffer-clear!)
      x))

  (define (yield! csv-fields)
    (yield (rev (cons (emit-field!) csv-fields)))
    (values 'start-field '()))

  (in-generator
    (for/fold ([state 'start-field]
               [csv-fields '()])
              ([c (in-input-port-chars in)])
      (case state
        [(start-field) 
         (cond [(char=? separator c) 
                (values 'start-field (cons (emit-field!) csv-fields))]
               [(char=? #\" c)
                (values 'in-quoted-field csv-fields)]
               [(record-separator? c) 
                (yield! csv-fields)]
               [else 
                 (buffer! c)
                 (values 'in-field csv-fields)])]
        [(in-field) 
         (cond [(char=? c separator) 
                (values 'start-field (cons (emit-field!) csv-fields))]
               [(record-separator? c) 
                (yield! csv-fields)]
               [else (buffer! c)
                     (values 'in-field csv-fields)])]
        [(in-quoted-field) 
         (cond [(char=? c #\")
                (values 'in-quoted-field-after-quote csv-fields)]
               [else (buffer! c)
                     (values 'in-quoted-field csv-fields)])]
        [(in-quoted-field-after-quote) 
         (cond [(char=? c #\")
                (buffer! c)
                (values 'in-quoted-field csv-fields)]
               [(char=? separator c)
                (values 'start-field (cons (emit-field!) csv-fields))]
               [(record-separator? c) 
                (yield! csv-fields)]
               [(raise (exn:fail "Expected double-quote or separator"
                                 (current-continuation-marks)))])]))))
                
        

