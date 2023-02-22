#lang racket

(require "let-parser.rkt")

(define sytanx-tree (to-ast (parse-let-syntax-tree "let a = -(7, 2) in - (a, 1)")))

(struct env ())
(struct empty-env env () #:transparent)
(struct extend-env env (id value parent) #:transparent)

(define (apply-env the-env var)
  (match the-env
    [(empty-env) (error "apply empty env")]
    [(extend-env id value parent)
       (cond
         [(eq? (ast-identifer-symbol var) (ast-identifer-symbol id)) value]
         [else (apply-env parent var)])]
    [_ error("type error")]))

(define init-env (empty-env ))

(define (value-of expr env)
  (match expr
    [(ast-number _) expr]
    [(ast-boolean _) expr]
    [(ast-identifer id) (apply-env env expr)]
    [(ast-if cond-expr true-expr false-expr)
     (let [(cond-value (value-of cond-expr env))]
       (match cond-value
         [(ast-boolean #true) (value-of true-expr env)]
         [(ast-boolean #false) (value-of false-expr env)]))]
    [(ast-in bind-id bind-value value-return)
     (value-of value-return
               (extend-env bind-id (value-of bind-value env) env))]
    [(ast-diff expr-1 expr-2) (ast-number (- (ast-number-n (value-of expr-1 env))
                                         (ast-number-n (value-of expr-2 env))))]))

(println (syntax-e to-ast))