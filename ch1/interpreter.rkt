#lang racket

(require "let-parser.rkt"
         megaparsack)

(define sytanx-tree (parse-let-syntax-tree "let a = -(3, 2) in - (a, 1)"))

