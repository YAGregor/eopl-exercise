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

(define simple-print "let x = 1 in print(x)")
(define simple-k-exp (out:parse "proc (x) x"))

(define (test-simple-print )
  (let* ([in-exp (in:parse simple-print)]
         [out-exp (cps-of-exp in-exp simple-k-exp)]
         [out-dump (dump out-exp)])
    (displayln out-exp)
    (displayln simple-print)
    (displayln " \n >>> after cps \n")
    (displayln out-dump)
    (out:run out-dump)))

(define print-tests
  (test-suite "print-tests"
              (test-case "simple"
                         (check-equal? 38 (test-simple-print )))))

(module+ test (run-tests print-tests))
