#lang racket

(require "cps-of-exp.rkt"
         rackunit
         rackunit/text-ui
         (prefix-in in: "cps-in-parser.rkt")
         (prefix-in in: "sytanx-struct.rkt")
         (prefix-in out: "cps-out-struct.rkt")
         (prefix-in out: "cps-out-parser.rkt"))

(define transform-basic "proc (x) x")
(define simple-k-exp (out:parse "proc (x) x"))

(define transform-tests
  (test-suite "transform-tests"
              (test-case "basic"
                         (println (cps-of-exp (in:parse transform-basic) simple-k-exp)))))

(module+ test (run-tests transform-tests))

