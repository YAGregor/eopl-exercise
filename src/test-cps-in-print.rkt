#lang racket
(require "interpreter.rkt" rackunit rackunit/text-ui)

(define print-source "let v = 1 in print(v)")

(define print-tests
  (test-suite
   "print-tests"
   (test-case "simple"
              (check-equal? 38 (run print-source)))))

(module+ test (run-tests print-tests))
