#lang racket/base
(require "../main.rkt"
         "list.rkt")

(provide %anywhere
         %anywhere/u
         %anywhere/a)

(define (%anywhere sexp %pred)
  (conde
    [(%pred sexp) succeed]
    [else
      (exist (fst snd)
        (%cons sexp fst snd)
        (anye (%anywhere fst %pred)
              (%anywhere snd %pred)))]))

(define (%anywhere/u sexp %pred)
  (condu
    [(%pred sexp) succeed]
    [else
      (exist (fst snd)
        (%cons sexp fst snd)
        (conde
          [(%anywhere/u fst %pred) succeed]
          [else (%anywhere/u snd %pred)]))]))

(define (%anywhere/a sexp %pred)
  (conda
    [(%pred sexp) succeed]
    [else
      (exist (fst snd)
        (%cons sexp fst snd)
        (conde
          [(%anywhere/a fst %pred) succeed]
          [else (%anywhere/a snd %pred)]))]))
