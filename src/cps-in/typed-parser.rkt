#lang typed/racket

(require "sytanx-struct.rkt")
(require/typed "parser.rkt" [parse (-> String Expression)])

(provide parse)
