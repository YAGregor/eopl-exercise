#lang racket
(require "interpreter.rkt" rackunit rackunit/text-ui)

(define test-proc-call "
let f = proc (x) begin set x = 2; +(x, 0) end in +(2, (f 1))
")

(define test-proc-rec "
letrec f(x) = if zero?(x) then 1 else +(-(x, 1), 1) in (f 4)
")


(define test-multi-arg "
let f = proc (x y) +(x, y) in (f 2 2)
")

(define test-no-arg "
let f = proc () 4 in (f )
")

(define test-id-ref "
let a = 3
    in let b = 4
        in let swap = proc (x) proc (y)
            let temp = deref(x)
                in begin
                    setref(x,deref(y));
                    setref(y,temp)
                    end
                in begin ((swap ref a) ref b); -(a,b) end
")

(define interpreter-tests
  (test-suite
   "tests for eopl interpreter"
   (test-case 
   "ref-435"
   (check-equal? 1 (run test-id-ref)))
   (test-case
    "test proc call"
    (check = 4 (run test-proc-call)))
   (test-case
    "test let rec"
    (check-equal? 4 (run test-proc-rec)))
   (test-case
    "simple multi arg"
    (check-equal? 4 (run test-multi-arg)))
   (test-case
    "no arg"
    (check-equal? 4 (run test-no-arg)))))

(module+ test
  (run-tests interpreter-tests))
