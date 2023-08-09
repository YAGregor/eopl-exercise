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

(define array-source "
let a = newarray(2,-99)
    p = proc (x)
        let v = arrayref(x,1)
        in arrayset(x,1,-(v,-1))
in begin arrayset(a,1,0); (p a); (p a); arrayref(a,1) end
")

(define interpreter-tests
  (test-suite
   "tests for eopl interpreter"
   (test-case
    "mutable-pair"
    (check = 88 (run source)))
   (test-case
   "mutable-array"
   (check = 0 (run array-source)))))

(module+ test
  (run-tests interpreter-tests))
