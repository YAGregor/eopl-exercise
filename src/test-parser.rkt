#lang racket

(require rackunit rackunit/text-ui "typed-parser.rkt")

(define basic-source-code "
let a = proc (v) if zero?(v) then 1 else 0 in (a 0)
")

(define parser-tests
  (test-suite "parser-tests"
              (test-case "basic"
                         (parse basic-source-code))))

(module+ test (run-tests parser-tests))
