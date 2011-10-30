#lang racket/base
(require "zebra.rkt" racket/pretty)
(pretty-display (zebrao))
(time (test-zebra 500))
