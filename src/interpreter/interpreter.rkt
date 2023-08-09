#lang racket

(require "parser.rkt" "built-in.rkt" "operations.rkt" "state.rkt")

(struct env ())
(struct empty-env env () #:transparent)
(struct extend-env env (id value parent) #:transparent)
(struct extend-env-rec env (name-proc-list parent) #:transparent)

(define (new-env-rec name-param-exp-list parent)
  (letrec ([name-proc-list
            (map (lambda (x)
                   (match x
                     [(name-param-exp n p e) (cons n (newref (procedure p e parent)))]))
                 name-param-exp-list)]
           [new-env
            (extend-env-rec name-proc-list parent)])
    (begin
      (for ([p name-proc-list])
        (match p
          [(cons name proc-ref)
           (set-procedure-env! (deref proc-ref) new-env)]))
      new-env)))


(struct name-param-exp (name param expression))

(define (apply-env the-env var)
  (match the-env
    [(extend-env id value parent)
     (cond
       [(eq? var id) value]
       [else (apply-env parent var)])]
    [(extend-env-rec name-procref-list parent)
     (let ([matched-proc-ref
            (findf
             (lambda (x)
               (match x
                 [(cons name proc-ref) (eq? name var)]))
             name-procref-list)])
       (cond [(false? matched-proc-ref) (apply-env parent var)]
             [else
              (match matched-proc-ref
                [(cons name proc-ref) proc-ref])]))]))

(define init-env (empty-env))

(struct procedure (param expression env) #:transparent #:mutable)

(define (value-of-proc param body env)
  (procedure param body env))


(define (value-of-begin-exp exp-list env)
  (match exp-list
    [(list last-exp) (value-of last-exp env)]
    [(list exp rest ...) (begin (value-of exp env) (value-of-begin-exp rest env))]))

(define (value-of-let-rec name-param-exp-list body env)
  (value-of body (new-env-rec
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
       (value-of exp (extend-env param (newref param-value) p-env))])))

(define (value-of expr env)
  (match expr
    [(ast-number v) v]
    [(ast-boolean v) v]
    [(ast-emptylist ) (eopl-empty-list )]
    [(ast-identifer id) (deref (apply-env env id))]
    [(ast-if cond-expr true-expr false-expr)
     (let [(cond-value (value-of cond-expr env))]
       (match cond-value
         [#t (value-of true-expr env)]
         [#f (value-of false-expr env)]))]
    [(ast-in (ast-identifer id) bind-value value-return)
     (value-of value-return
               (extend-env id (newref (value-of bind-value env)) env))]
    [(ast-operation name parameters) (value-of-op name (map (lambda (v) (value-of v env)) parameters))]
    [(ast-proc (ast-identifer identifier) expression) (value-of-proc identifier expression env)]
    [(ast-proc-call expression param) (value-of-proc-call expression param env)]
    [(ast-begin exp-list) (value-of-begin-exp exp-list env)]
    [(ast-let-rec name-param-exp-list body) (value-of-let-rec name-param-exp-list body env)]
    [(ast-assign (ast-identifer id) expression) (setref! (apply-env env id) (value-of expression env))]))

(define (interpret-statement statement env)
  (match statement
    [(statement-assign (ast-identifer id) expression)
     (setref! (apply-env env id) (value-of expression env))]
    [(statement-print expression) (println (value-of expression env ))]
    [(statement-if cond-expression true-statement false-statement)
     (cond [(value-of cond-expression env) (interpret-statement true-statement env)]
           [else (interpret-statement false-statement env)])]
    [(statement-while cond-expression do-statement)
     (cond [(value-of cond-expression env)
            (begin
              (interpret-statement do-statement env)
              (interpret-statement statement env))]
           [else null])]
    [(statement-var identifier-list body-statement)
     (let* ([all-var-list (map ast-identifer-symbol identifier-list)]
            [new-env (letrec ([gen-new-env
                               (lambda (id-list)
                                 (match id-list
                                   [(list) env]
                                   [(list id rest ...)
                                    (extend-env id (newref null) (gen-new-env rest))]))])
                       (gen-new-env all-var-list))])
       (interpret-statement body-statement new-env))]
    [(statement-block statement-list)
     (for ([s statement-list])
       (interpret-statement s env))]))

(define (value-of-source source)
  (begin
    (initialize-the-store!)
    (interpret-statement (let ([ast (parse source)])
                           (println (list "ast!!-->" ast))
                           ast)
                         init-env)))

(println (value-of-source "
var f,x; {f = proc (x) proc(y) *(x,y);
x = 3;
print ((f 4) x)}"))
