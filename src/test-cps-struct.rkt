#lang racket

(require "cps-of-exp.rkt"
         "dump-cps-out.rkt"
         "interpreter.rkt"
         rackunit
         rackunit/text-ui
         (prefix-in in: "cps-in-parser.rkt")
         (prefix-in in: "sytanx-struct.rkt")
         (prefix-in out: "cps-out-struct.rkt")
         (prefix-in out: "cps-out-parser.rkt")
         (prefix-in out: "cps-out-interpreter.rkt"))

(define transform-basic "proc (x) x")
(define simple-k-exp (out:parse "proc (x) x"))

(define fib-code "
letrec fib (n) = if zero?(-(n, 1)) then 1
                             else
                                 if zero?(-(n, 2)) then 1
                                                  else -((fib -(n, 1)),
                                                         -(0, (fib -(n, 2))))
                in (fib 6)
")

(define (test-fib )
  (let* ([in-exp (in:parse fib-code)]
         [out-exp (cps-of-exp in-exp simple-k-exp)]
         [out-dump (dump out-exp)])
    (displayln out-exp)
    (displayln fib-code)
    (displayln " \n >>> after cps... \n")
    (displayln out-dump)
    (newline)
    (out:run out-dump)))

(define transform-tests
  (test-suite "transform-tests"
              (test-case "simpler"
                         (check-equal? 8 (test-fib)))))

(module+ test (run-tests transform-tests))
