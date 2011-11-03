#lang racket/base
(require "../main.rkt"
         "list.rkt")

(provide %anywhere)

(define (%anywhere sexp %pred)
  (conde
    [(%pred sexp) succeed]
    [else
      (exist (fst snd)
        (%cons fst snd sexp)
        (anye (%anywhere fst %pred)
              (%anywhere snd %pred)))]))
