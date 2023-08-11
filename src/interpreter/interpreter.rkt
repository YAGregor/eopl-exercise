#lang racket

(require "parser.rkt" "built-in.rkt" "operations.rkt" "state.rkt")

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
       (cond [(false? matched-n-p-e) (apply-env parent var)]
             [else
              (match matched-n-p-e
                [(name-param-exp n p e) (newref (procedure (list p) e the-env))])]))]))

(define init-env (empty-env))

(struct procedure (param-list expression env) #:transparent)

(struct trunk (expression env))

(define (value-of-proc param-list body env)
  (procedure (map ast-identifer-symbol param-list) body env))


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

(define (value-of-operand param env)
  (match param
    [(ast-identifer id) (newref (deref (apply-env env id)))]
    [(? ast-number? ast-proc?) (newref (value-of param env))]
    [_ (newref (trunk param env))]))


(define (value-of-proc-call proc param-list env)
  (let ([proc-value (value-of proc env)]
        [param-bind (map (lambda (oprand) (value-of-operand oprand env)) param-list)])
    (match proc-value
      [(procedure p-param-list exp p-env)
       (value-of
        exp
        (foldl
         (lambda (p-param ref env) (extend-env p-param ref env))
         p-env p-param-list param-bind))])))

(define (extend-env-id-exp-list id-exp-list env)
  (match id-exp-list
    [(list ) env]
    [(list (cons
            (ast-identifer id) expression)
           rest ...)
     (extend-env id
                 (newref (value-of expression env))
                 (extend-env-id-exp-list rest env))]))

(define (value-of-variable identifier env)
  (let* ([var-ref (apply-env env identifier)]
         [retreived (deref var-ref)])
    (match retreived
      [(trunk expression env)
       (let ([trunk-value (value-of expression env)])
         (setref! var-ref trunk-value)
         trunk-value)]
      [_ retreived])))

(define (value-of expr env)
  (match expr
    [(ast-number v) v]
    [(ast-boolean v) v]
    [(ast-emptylist ) (eopl-empty-list )]
    [(ast-identifer id)  (value-of-variable id env)]
    [(ast-if cond-expr true-expr false-expr)
     (let [(cond-value (value-of cond-expr env))]
       (match cond-value
         [#t (value-of true-expr env)]
         [#f (value-of false-expr env)]))]
    [(ast-let id-exp-list value-return) (value-of value-return (extend-env-id-exp-list id-exp-list env))]
    [(ast-assign (ast-identifer id) value-exp) (setref! (apply-env env id) (value-of value-exp env))]
    [(ast-operation name parameters) (value-of-op name (map (lambda (v) (value-of v env)) parameters))]
    [(ast-proc identifier-list expression) (value-of-proc identifier-list expression env)]
    [(ast-proc-call expression param-list) (value-of-proc-call expression param-list env)]
    [(ast-begin exp-list) (value-of-begin-exp exp-list env)]
    [(ast-let-rec name-param-exp-list body) (value-of-let-rec name-param-exp-list body env)]))

(define (run source)
  (begin
    (initialize-the-store!)
    (value-of (parse source)
              init-env)))

(provide run)
