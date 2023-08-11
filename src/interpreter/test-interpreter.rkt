#lang racket
(require "interpreter.rkt" rackunit rackunit/text-ui)

(define test-call-by-need "
letrec infinite-loop (x) = (infinite-loop -(x,-1))
    in let f = proc (z) 11
        in (f (infinite-loop 0))
")

(run test-call-by-need)

(define interpreter-tests
  (test-suite
   "tests for eopl interpreter"
   (test-case
   "basic call by need"
   (check-equal? 11 (run test-call-by-need)))))

(module+ test
  (run-tests interpreter-tests))
