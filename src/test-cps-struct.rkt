#lang racket

(require "cps-of-exp.rkt"
         "dump-cps-out.rkt"
         rackunit
         rackunit/text-ui
         (prefix-in in: "cps-in-parser.rkt")
         (prefix-in in: "sytanx-struct.rkt")
         (prefix-in out: "cps-out-struct.rkt")
         (prefix-in out: "cps-out-parser.rkt"))

(define transform-basic "proc (x) x")
(define simple-k-exp (out:parse "proc (x) x"))

(define fib-code "
letrec fib (n) = if zero?(-(n, 1)) then 1
                             else
                                 if zero?(-(n, 2)) then 1
                                                  else -((fib -(n, 1)),
                                                         -(0, (fib -(n, 2))))
                in (fib 5)
")

(define transform-tests
  (test-suite "transform-tests"
              (test-case "basic"
                         (println (cps-of-exp (in:parse transform-basic) simple-k-exp)))
              (test-case "fib"
                         (println (cps-of-exp (in:parse fib-code) simple-k-exp)))
              (test-case "dump-fib"
                         (println
                          (dump
                           (cps-of-exp
                            (in:parse fib-code) simple-k-exp))))))

(module+ test (run-tests transform-tests))

