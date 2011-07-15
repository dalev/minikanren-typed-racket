#lang racket/base
(require racket/contract
         racket/match)

(define nonempty-string/c
  (flat-named-contract 'nonempty-string/c
    (lambda (s) (and (string? s) (not (zero? (string-length s)))))))

(struct rhs (in-trie? subtrie) #:transparent)

(define trie/c (hash/c char? rhs?))

;; CR dalev: there's actually no reason to restrict to strings; any sequence
;; will do if we just remove uses of in-string and favor generic iterators.
;; I wonder what the performance difference will be on /usr/share/dict/words.
(provide/contract
  [create (-> trie/c)]
  [insert! (trie/c nonempty-string/c . -> . trie/c)]
  [contains? (trie/c nonempty-string/c . -> . boolean?)]
  [of-file (() (#:path path?) . ->* . trie/c)])

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
