#lang racket
(require "interpreter.rkt" rackunit rackunit/text-ui)

(define test-call-by-need "
letrec infinite-loop (x) = (infinite-loop -(x,-1))
    in let f = proc (z) 11
        in (f (infinite-loop 0))
")

(define test-yc-lazy "
let makerec = proc (f)
    let d = proc (x) (f (x x))
        in (f (d d))
    in let maketimes4 = proc (f) proc (x)
        if zero?(x)
            then 0
            else -((f -(x,1)), -4)
in let times4 = (makerec maketimes4)
    in (times4 3)
")

(define test-diff-anwser "
let n = 1 in 
    let change = proc () setref(n, 2)
        f = proc (a b) b
        in (f (change) n)
")


(define interpreter-tests
  (test-suite
   "tests for eopl interpreter"
   (test-case
   "diff anwser"
   (check-equal? 1 (run test-diff-anwser)))
   (test-case 
   "lazy yc"
   (check-equal? 12 (run test-yc-lazy)))
   (test-case
   "basic call by need"
   (check-equal? 11 (run test-call-by-need)))))

(module+ test
  (run-tests interpreter-tests))
