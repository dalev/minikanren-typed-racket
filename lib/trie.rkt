#lang racket/base
(require racket/contract
         racket/match
         racket/generator)

(define nonempty-string/c
  (flat-named-contract 'nonempty-string/c
    (lambda (s) (and (string? s) (not (zero? (string-length s)))))))

(struct rhs (in-trie? subtrie) #:transparent)

(define trie/c (hash/c char? rhs?))

;; CR dalev: there's actually no reason to restrict to strings; any sequence
;; will do if we just remove uses of in-string and favor generic iterators.
;; I wonder what the performance difference will be on /usr/share/dict/words.
;; One difficulty though will be `in-trie':  it needs to pick a concrete
;; sequence type to return.  Maybe vectors are good enough for the general
;; case.  Could provide a specialization for strings.
(provide/contract
  [create (-> trie/c)]
  [insert! (trie/c nonempty-string/c . -> . trie/c)]
  [contains? (trie/c nonempty-string/c . -> . boolean?)]
  [in-trie (trie/c . -> . sequence?)]
  [of-file (() (#:path path?) . ->* . trie/c)])

(define (in-trie t)
  (define buffer (make-string 256))
  (define idx 0)
  (define (current-string) (substring buffer 0 idx))

  (define (double-buffer!)
    (let* ([current-length (string-length buffer)]
           [new-buffer (make-string (* 2 current-length))])
      (string-copy! new-buffer 0 buffer)
      (set! buffer new-buffer)))

  (define (push! c) 
    (string-set! buffer idx c) 
    (set! idx (add1 idx))
    (when (>= idx (string-length buffer))
      (double-buffer!)))

  (define (pop!) (set! idx (sub1 idx)) (string-ref buffer idx))

  (in-generator
    (let loop ([t t])
      (for ([(char rhs) (in-hash t)])
        (push! char)
        (when (rhs-in-trie? rhs) (yield (current-string)))
        (loop (rhs-subtrie rhs))
        (pop!)))))

(define (create) (make-hasheqv))

(define (contains? t word)
  (when (string=? word "") (error 'contains? "expected nonempty word" word))
  (let/ec return
    (let-values ([(_final-trie in-trie?)
                  (for/fold ([t t] [in-trie? #f]) ([c (in-string word)])
                    (match (hash-ref t c 'none)
                      ['none (return #f)]
                      [(rhs in-trie? subtrie)
                       (values subtrie in-trie?)]))])
      in-trie?)))

(define (insert! t word)
  (define max-index (sub1 (string-length word)))
  (when (< max-index 0) (error 'insert "Expected nonempty word" word))
  (for/fold ([t t]) ([c (in-string word)] 
                     [i (in-naturals 0)])
    (define at-last-character? (= i max-index))
    (match (hash-ref t c 'none)
      ['none
       (let ([subtrie (create)])
         (hash-set! t c (rhs at-last-character? subtrie))
         subtrie)]
      [(rhs in-trie? subtrie)
       (when (and at-last-character? 
                  (not in-trie?))
         (hash-set! t c (rhs #t subtrie)))
        subtrie]))
  t)

(define (of-file #:path [path (build-path "/" "usr" "share" "dict" "words")])
  (let ([trie (create)])
    (with-input-from-file path
      (lambda ()
        (for ([line (in-lines)]) (insert! trie line))
        trie))))
