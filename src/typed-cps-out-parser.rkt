#lang typed/racket

(require "sytanx-struct.rkt")
(require/typed "cps-out-parser.rkt" [parse (-> String Expression)])

(provide parse)
