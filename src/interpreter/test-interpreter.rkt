#lang racket
(require "interpreter.rkt" rackunit rackunit/text-ui)

(define source "
let glo = pair(11,22)
    in let f = proc (loc)
        let d1 = setright(loc, left(loc))
            in let d2 = setleft(glo, 99)
        in -(left(loc),right(loc))
in (f glo)
")

(define interpreter-tests
  (test-suite
   "tests for eopl interpreter"
   (test-case
    "mutable-pair"
    (check = 88 (run source)))))

(module+ test
  (run-tests interpreter-tests))
