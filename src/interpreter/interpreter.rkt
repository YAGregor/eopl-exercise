#lang racket

(require "parser.rkt")
(require "built-in.rkt")

(struct env ())
(struct empty-env env () #:transparent)
(struct extend-env env (id value parent) #:transparent)

(define invalid-params "invalid params")

(define (op-zero? params)
  (match params
    [(list (struct ast-number (n))) (= n 0)]
    [_ (error invalid-params)]))

(define (op-minus params)
  (match params
    [(list (ast-number n)) (ast-number (- n))]
    [_ (error invalid-params)]))

(define (op-equals? params)
  (match params
    [(list v1 v2) (ast-boolean (equal? v1 v2))]
    [_ (error invalid-params)]))

(define (op-greater? params)
  (match params
    [(list (ast-number v1) (ast-number v2)) (ast-boolean (> v1 v2))]
    [_ (error invalid-params)]))

(define (op-less? params)
  (match params
    [(list (ast-number v1) (ast-number v2)) (ast-boolean (< v1 v2))]))

(define (op-+ params)
  (match params
    [(list (ast-number v1) (ast-number v2)) (ast-number (+ v1 v2))]))

(define (op-- params)
  (match params
    [(list (ast-number v1) (ast-number v2)) (ast-number (- v1 v2))]
    [_ (error invalid-params)]))

(define (op-* params)
  (match params
    [(list (ast-number v1) (ast-number v2)) (ast-number (* v1 v2))]))

(define (op-/ params)
  (match params
    [(list (ast-number v1) (ast-number v2)) (ast-number (/ v1 v2))]))

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


(define (apply-env the-env var)
  (match the-env
    [(empty-env) (error "apply empty env")]
    [(extend-env id value parent)
     (cond
       [(eq? (ast-identifer-symbol var) (ast-identifer-symbol id)) value]
       [else (apply-env parent var)])]
    [_ error("type error")]))

(define init-env  (empty-env ))

(define (value-of expr env)
  (match expr
    [(ast-number _) expr]
    [(ast-boolean _) expr]
    [(ast-identifer id) (apply-env env expr)]
    [(ast-emptylist ) (eopl-empty-list )]
    [(ast-proc identifier expression) (ast-proc identifier expression)]
    [(ast-if cond-expr true-expr false-expr)
     (let [(cond-value (value-of cond-expr env))]
       (match cond-value
         [(ast-boolean #true) (value-of true-expr env)]
         [(ast-boolean #false) (value-of false-expr env)]))]
    [(ast-in bind-id bind-value value-return)
     (value-of value-return
               (extend-env bind-id (value-of bind-value env) env))]
    [(ast-operation name parameters) (value-of-op name (map (lambda (v) (value-of v env)) parameters))]))

(define (value-of-source source) (value-of (parse source) empty-env))
(println (value-of-source "
let x = 4
  in car(cdr(cdr(list(1, 2, 3, x, -(x, 3)))))
"))
