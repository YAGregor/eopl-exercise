#lang typed/racket

(require "ast-element.rkt")
(require/typed "parser.rkt" [parse (-> String expression)])

(provide parse)