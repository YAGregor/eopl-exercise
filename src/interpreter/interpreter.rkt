#lang racket

(require "parser.rkt")
(require "built-in.rkt")
(require "operations.rkt")

(struct env ())
(struct empty-env env () #:transparent)
(struct extend-env env (id value parent) #:transparent)
(struct extend-env-rec env (proc-name param-identifier body parent) #:transparent)

(define (apply-env the-env var)
  (match the-env
    [(extend-env id value parent)
     (cond
       [(eq? var id) value]
       [else (apply-env parent var)])]
    [(extend-env-rec proc-name param-identifier expression parent)
     (cond
       [(eq? var proc-name) (procedure param-identifier expression the-env)]
       [else (apply-env parent var)])]))


(define (value-of-op op-name params)
  (match op-name
    ['zero? (op-zero? params)]
    ['minus (op-minus params)]
    ['equal? (op-equals? params)]
    ['greater? (op-greater? params)]
    ['less? (op-less? params)]
    ['+ (op-+ params)]
    ['- (op-- params)]
    ['* (op-* params)]
    ['/ (op-/ params)]
    ['cons (op-cons params)]
    ['list (op-list params)]
    ['car (op-car params)]
    ['cdr (op-cdr params)]))


(define init-env (empty-env))

(struct procedure (param expression env) #:transparent)

(define (value-of-proc param body env)
  (procedure param body env))

(define (apply-proc proc param-value env)
  (match proc
    [(procedure param expression procedure-env)
     (value-of expression (extend-env param param-value procedure-env))]))

(define (value-of-proc-call procedure param env)
  (apply-proc procedure param env))


(define (value-of expr env)
  (match expr
    [(ast-number v) v]
    [(ast-boolean v) v]
    [(ast-identifer id) (apply-env env id)]
    [(ast-emptylist ) (eopl-empty-list )]
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
    [(ast-proc-call expression param) (value-of-proc-call (value-of expression env) (value-of param env) env)]
    [(ast-let-rec (ast-identifer name) (ast-identifer param-id) expression body)
     (value-of body (extend-env-rec name param-id expression env))]))

(define (value-of-source source)
  (value-of (let ([ast (parse source)])
              (println (list "ast!!-->" ast))
              ast)
            init-env))

(println (value-of-source "letrec f(x) = if equal?(x, 1) then 1 else +(x, (f -(x, 1))) in (f 3)"))
