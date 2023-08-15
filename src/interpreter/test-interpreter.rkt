#lang racket
(require "cont-interpreter.rkt" "built-in.rkt" rackunit rackunit/text-ui)

(define basic-let "let x = 1 in x")
(define multi-branch-let "let x = 1 y = 2 in x")
(define test-opreration "let x = 1 y = 1 in list(x,y)")
(define test-if "let x = 1 in if zero?(x) then 2 else 3")

(define interpreter-tests
  (test-suite
   "tests for eopl interpreter"
   (test-case 
   "if"
   (check-equal? 3 (run test-if)))
   (test-case
   "operation call"
   (check-equal? (exp-list-pair-value (run test-opreration)) 1))
   (test-case
    "multi branch let"
    (check-equal? 1 (run multi-branch-let)))
   (test-case
    "basic let"
    (check-equal? 1 (run basic-let)))))

(module+ test
  (run-tests interpreter-tests))
