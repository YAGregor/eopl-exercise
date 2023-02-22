#lang racket

(require "let-parser.rkt")

(define sytanx-tree (to-ast (parse-let-syntax-tree "let a = -(3, 2) in - (a, 1)")))

(struct env ())
(struct empty-env env ())
(struct extend-env env (id value parent))

(define (apply-env the-env var)
  (match the-env
    [(empty-env) (error "apply empty env")]
    [(extend-env id value parent)
     (cond
       [(equal? var id) value]
       [else (apply-env parent var)])]
    [_ error("type error")]))

(define init-env
  (extend-env 'a (let-number 1)
              (extend-env 'b (let-number 2)
                          (empty-env))))

(define (value-of expr env)
  (match expr
    [(let-number _) expr]
    [(let-boolean _) expr]
    [(identifer id) (apply-env env id)]
    [(let-if cond-expr true-expr false-expr)
     (let [(cond-value (value-of cond-expr env))]
       (match cond-value
         [(let-boolean #true) (value-of true-expr env)]
         [(let-boolean #false) (value-of false-expr env)]))]
    [(let-in id value-bind value-return)
     (value-of value-return (extend-env id (value-of value-bind env)))]
    [(diff expr-1 expr-2) (let-number (- (let-number-n (value-of expr-1 env))
                                         (let-number-n (value-of expr-2 env))))]))

(define value-test (value-of sytanx-tree init-env))
