#lang racket

(require "let-parser.rkt"
         megaparsack)

(define test-program (parse-let-syntax-tree "let a = -(3, 2) in - (a, 1)"))

(println  (map syntax-e (syntax->list test-program)))
(println (to-ast  test-program))
