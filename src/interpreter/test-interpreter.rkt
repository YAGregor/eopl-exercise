#lang racket
(require "continuation-interpreter.rkt" rackunit rackunit/text-ui)

(define basic-let "let x = 1 in x")
(define multi-branch-let "let x = 1 y = 2 in x")


(define interpreter-tests
  (test-suite
   "tests for eopl interpreter"
   (test-case
    "multi branch let"
    (check-equal? 1 (run multi-branch-let)))
   (test-case
    "basic let"
    (check-equal? 1 (run basic-let)))))

(module+ test
  (run-tests interpreter-tests))
