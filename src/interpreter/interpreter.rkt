#lang racket

(require "parser.rkt" "built-in.rkt" "operations.rkt")

(struct env ())
(struct empty-env env () #:transparent)
(struct extend-env env (id value parent) #:transparent)
(struct extend-env-rec env (name-param-exp-list parent) #:transparent)

(struct name-param-exp (name param expression))

(define (apply-env the-env var)
  (match the-env
    [(extend-env id value parent)
     (cond
       [(eq? var id) value]
       [else (apply-env parent var)])]
    [(extend-env-rec name-param-exp-list parent)
     (let ([matched-n-p-e
            (findf
             (lambda (x)
               (match x
                 [(name-param-exp n p e) (eq? n var)]))
             name-param-exp-list)])
       (cond [(null? matched-n-p-e) (apply-env parent var)]
             [else
              (match matched-n-p-e
                [(name-param-exp n p e) (procedure p e the-env)])]))]))

(define init-env (empty-env))

(struct procedure (param expression env) #:transparent)

(define (value-of-proc param body env)
  (procedure param body env))


(define (value-of-begin-exp exp-list env)
  (match exp-list
    [(list last-exp) (value-of last-exp env)]
    [(list exp rest ...) (begin (value-of exp env) (value-of-begin-exp rest env))]))

(define (value-of-let-rec name-param-exp-list body env)
  (value-of body (extend-env-rec
                  (map
                   (lambda (x)
                     (match x [(ast-name-param-exp (ast-identifer n) (ast-identifer p) e) (name-param-exp n p e)]))
                   name-param-exp-list)
                  env)))

(define (value-of-proc-call proc param env)
  (let ([proc-value (value-of proc env)]
        [param-value (value-of param env)])
    (match proc-value
      [(procedure param exp p-env)
       (value-of exp (extend-env param param-value p-env))])))

(define (value-of expr env)
  (match expr
    [(ast-number v) v]
    [(ast-boolean v) v]
    [(ast-emptylist ) (eopl-empty-list )]
    [(ast-identifer id) (apply-env env id)]
    [(ast-if cond-expr true-expr false-expr)
     (let [(cond-value (value-of cond-expr env))]
       (match cond-value
         [#t (value-of true-expr env)]
         [#f (value-of false-expr env)]))]
    [(ast-in (ast-identifer id) bind-value value-return)
     (value-of value-return
               (extend-env id (value-of bind-value env) env))]
    [(ast-operation name parameters) (value-of-op name (map (lambda (v) (value-of v env)) parameters))]
    [(ast-proc (ast-identifer identifier) expression) (value-of-proc identifier expression env)]
    [(ast-proc-call expression param) (value-of-proc-call expression param env)]
    [(ast-begin exp-list) (value-of-begin-exp exp-list env)]
    [(ast-let-rec name-param-exp-list body) (value-of-let-rec name-param-exp-list body env)]))

(define (value-of-source source)
  (value-of (let ([ast (parse source)])
              (println (list "ast!!-->" ast))
              ast)
            init-env))

(println (value-of-source "
letrec f(x) = if zero?(x) then 1 else +(2, (g -(x, 1)))
       g(x) = if zero?(x) then 1 else +(2, (f -(x, 1)))
        in begin 1; 2; (f 5) end
"))
