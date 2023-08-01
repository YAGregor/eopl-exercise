#lang racket

(require "parser.rkt")
(require "built-in.rkt")

(struct env ())
(struct empty-env env () #:transparent)
(struct extend-env env (id value parent) #:transparent)
(struct rec-extend-env env (parent) #:transparent)

(define (apply-env the-env var)
  (match the-env
    [(empty-env) (error "apply empty env")]
    [(extend-env id value parent)
     (cond
       [(eq? (ast-identifer-symbol var) (ast-identifer-symbol id)) value]
       [else (apply-env parent var)])]
    [_ error("type error")]))

(define invalid-params "invalid params")

(define (op-zero? params)
  (match params
    [(list (? number? n)) (= n 0)]
    [_ (error invalid-params)]))

(define (op-minus params)
  (match params
    [(list (? number? n)) (- n)]
    [_ (error invalid-params)]))

(define (op-equals? params)
  (match params
    [(list v1 v2) (equal? v1 v2)]
    [_ (error invalid-params)]))

(define (op-greater? params)
  (match params
    [(list (? number? v1) (? number? v2)) (> v1 v2)]
    [_ (error invalid-params)]))

(define (op-less? params)
  (match params
    [(list (? number? v1) (? number? v2)) (< v1 v2)]))

(define (op-+ params)
  (match params
    [(list (? number? v1) (? number? v2)) (+ v1 v2)]))

(define (op-- params)
  (match params
    [(list (? number? v1) (? number? v2)) (- v1 v2)]
    [_ (error invalid-params)]))

(define (op-* params)
  (match params
    [(list (? number? v1) (? number? v2)) (* v1 v2)]))

(define (op-/ params)
  (match params
    [(list (? number? v1) (? number? v2)) (/ v1 v2)]))

(define (op-cons params)
  (match params
    [(list  v1 v2) (eopl-pare v1 v2)]))

(define (op-list params)
  (match params
    [(list ) (eopl-empty-list )]
    [(list head rest ...) (eopl-pare head (op-list rest))]))

(define (op-car params)
  (match params
    [(list (eopl-pare head rest) ) head]))

(define (op-cdr params)
  (match params
    [(list (eopl-pare head rest)) rest]))


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


(define init-env  (empty-env))

(struct procedure (param expression env) #:transparent)
(struct procedure-rec (proc-name proc-identifier expression env) #:transparent)

(define (value-of-proc param-name-list body env)
  (procedure param-name-list body env))

(define (apply-proc proc param-value env)
  (match proc
    [(procedure param expression procedure-env) (value-of expression (extend-env param param-value procedure-env))]))

(define (value-of-proc-call procedure param env)
 (apply-proc procedure param env))

(define (value-of expr env)
  (match expr
    [(ast-number v) v]
    [(ast-boolean v) v]
    [(ast-identifer id) (apply-env env expr)]
    [(ast-emptylist ) (eopl-empty-list )]
    [(ast-if cond-expr true-expr false-expr)
     (let [(cond-value (value-of cond-expr env))]
       (match cond-value
         [#t (value-of true-expr env)]
         [#f (value-of false-expr env)]))]
    [(ast-in bind-id bind-value value-return)
     (value-of value-return
               (extend-env bind-id (value-of bind-value env) env))]
    [(ast-operation name parameters) (value-of-op name (map (lambda (v) (value-of v env)) parameters))]
    [(ast-proc identifier expression) (value-of-proc identifier expression env)]
    [(ast-proc-call expression param) (value-of-proc-call (value-of expression env) (value-of param env) env)]))

(define (value-of-source source)
  (value-of (let ([ast (parse source)])
              (println ast)
              ast)
            empty-env))

(println (value-of-source "zero?(1)"))
