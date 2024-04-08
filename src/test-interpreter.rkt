#lang racket

(require "interpreter.rkt" rackunit rackunit/text-ui)

(define basic-source-code "
let a = proc (v) if zero?(v) then 1 else 0 in (a 0)
")

(define fib-code "
letrec fib (n) = if zero?(-(n, 1)) then 1
                             else
                                 if zero?(-(n, 2)) then 1
                                                  else -((fib -(n, 1)),
                                                         -(0, (fib -(n, 2))))
                in (fib 5)
")

(define interpreter-tests
  (test-suite "interpreter-tests"
              (test-case "basic"
                         (check-equal? (run basic-source-code) 1))
              (test-case "fib"
                         (check-equal? (run fib-code) 5))))

(module+ test (run-tests interpreter-tests))
