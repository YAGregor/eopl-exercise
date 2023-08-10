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

(define test-call-by-ref "
let b = 3
    in let p = proc (x) proc(y)
                    begin
                        set x = 4;
                        y
                    end
                in ((p b) b)
")

(define test-multi-arg "
let f = proc (x y) +(x, y) in (f 2 2)
")

(define test-no-arg "
let f = proc () 4 in (f )
")

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
    (check-equal? 4 (run test-proc-rec)))
   (test-case
    "test call ref"
    (check-equal? 4 (run test-call-by-ref)))
   (test-case
    "simple multi arg"
    (check-equal? 4 (run test-multi-arg)))
    (test-case
    "no arg"
    (check-equal? 4 (run test-no-arg)))))

(module+ test
  (run-tests interpreter-tests))
