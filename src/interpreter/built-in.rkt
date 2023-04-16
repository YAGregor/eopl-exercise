#lang racket

(struct atom (value))

(struct eopl-emtyp-list ())

(struct eopl-list (value next))

(define default-empty-list (eopl-emtyp-list ))


(provide default-empty-list)
