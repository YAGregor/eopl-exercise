#lang racket

(struct eopl-list ())

(struct eopl-empty-list eopl-list () #:transparent)

(struct eopl-pare eopl-list (value next) #:transparent)

(define default-empty-list (eopl-empty-list ))


(provide default-empty-list (struct-out eopl-pare) (struct-out eopl-empty-list) (struct-out eopl-list))
