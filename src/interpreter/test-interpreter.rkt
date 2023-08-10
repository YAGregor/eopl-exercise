#lang racket
(require "interpreter.rkt" rackunit rackunit/text-ui)

(define test-let-set "
let x = 1 y = 2 in begin set x = 2; +(x, y) end
")

(define test-proc-call "
let f = proc (x) begin set x = 2; +(x, 0) end in +(2, (f 1))
")

(define test-proc-rec "
letrec f(x) = if zero?(x) then 1 else +(-(x, 1), 1) in (f 4)
")

(run test-proc-rec)

(define interpreter-tests
  (test-suite
   "tests for eopl interpreter"
   (test-case
    "test let and set assign"
    (check = 4 (run test-let-set)))
   (test-case
    "test proc call"
    (check = 4 (run test-proc-call)))
   (test-case
    "test let rec"
    (check-equal? 4 (run test-proc-rec)))))

(module+ test
  (run-tests interpreter-tests))
