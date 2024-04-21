#lang racket

(require "cps-out-interpreter.rkt" rackunit rackunit/text-ui)

(define print-k-source "print-k(1);let x = 2 in x")

(define print-tests
  (test-suite "print-tests"
              (test-case "simple" (check-equal? 2 (run print-k-source)))))

(module+ test
  (run-tests print-tests))
