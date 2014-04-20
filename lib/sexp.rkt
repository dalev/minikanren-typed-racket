#lang racket/base
(require "kanren.rkt"
         "list.rkt")

(provide %anywhere
         %anywhere/u
         %anywhere/a)

(define (%anywhere sexp %pred)
  (conde
    [(%pred sexp) succeed]
    [else
      (fresh (fst snd)
        (%cons sexp fst snd)
        (anye (%anywhere fst %pred)
              (%anywhere snd %pred)))]))

;; CR dalev: [%anywhere/u] and [%anywhere/a] don't seem quite right
(define (%anywhere/u sexp %pred)
  (condu
    [(%pred sexp) succeed]
    [else
      (fresh (fst snd)
        (%cons sexp fst snd)
        (conde
          [(%anywhere/u fst %pred) succeed]
          [else (%anywhere/u snd %pred)]))]))

(define (%anywhere/a sexp %pred)
  (conda
    [(%pred sexp) succeed]
    [else
      (fresh (fst snd)
        (%cons sexp fst snd)
        (conde
          [(%anywhere/a fst %pred) succeed]
          [else (%anywhere/a snd %pred)]))]))
