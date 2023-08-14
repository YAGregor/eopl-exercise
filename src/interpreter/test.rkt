#lang typed/racket

(define t-l (list 1 2 3))
(define t (findf (lambda (v) (eq? v 1)) t-l))
(println t)