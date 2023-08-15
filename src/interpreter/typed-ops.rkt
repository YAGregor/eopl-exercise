#lang typed/racket
(require (only-in "built-in.rkt" ExpVal))
(require/typed "operations.rkt" [value-of-op (-> Symbol (Listof ExpVal) ExpVal)])

(provide value-of-op)
