#lang typed/racket

(require "cps-out-struct.rkt")
(require/typed "cps-out-parser.rkt" [parse (-> String Expression)])

(provide parse)
