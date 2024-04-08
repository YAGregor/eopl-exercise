#lang typed/racket

(require "sytanx-struct.rkt")
(require/typed "cps-in-parser.rkt" [parse (-> String Expression)])

(provide parse)
