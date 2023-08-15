#lang typed/racket

(require "ast-element.rkt" "typed-parser.rkt" "typed-ops.rkt"
         (only-in "built-in.rkt" env extend-env extend-env-rec empty-env name-param-exp name-param-exp-name procedure ExpVal))

(struct cont ())
(struct end-cont cont ())
(struct if-cont cont ([true-exp : expression] [false-exp : expression] [parent-cont : cont]))
(struct let-cont cont ([identifiers : (Listof Symbol)] [exps : (Listof expression)] [body-exp : expression] [parent-cont : cont]))
(struct op-cont cont ([name : Symbol] [exist-exp-val : (Listof ExpVal)] [exps : (Listof expression)] [parent-cont : cont]))
(struct proc-cont cont ())


(: apply-env (-> env Symbol ExpVal))
(define (apply-env applied-env id)
  (match applied-env
    [(extend-env parent extend-id value)
     (cond [(equal? id extend-id) value]
           [else (apply-env parent id)])]
    [(extend-env-rec parent name-param-exp-list)
     (match (findf (compose (curry equal? id) name-param-exp-name) name-param-exp-list)
       [(name-param-exp n p exp) (procedure (list p) exp applied-env)]
       [_ (apply-env parent id)])]))

(: apply-cont (-> cont ExpVal env ExpVal))
(define (apply-cont cont-applied exp-val-applied env-applied)
  (match cont-applied
    [(? end-cont?) (begin (printf "end of computation ~s \n" exp-val-applied) exp-val-applied)]
    [(if-cont true-exp false-exp parent-cont)
     (cond [exp-val-applied (value-of/k true-exp env-applied parent-cont)]
           [else (value-of/k false-exp env-applied parent-cont)])]
    [(let-cont ids exps body parent-cont)
     (match exps
       [(list ) (value-of/k body (extend-env env-applied (car ids) exp-val-applied) parent-cont) ]
       [(list first-exp rest-exp ...)
        (value-of/k first-exp
                    (extend-env env-applied (car ids) exp-val-applied)
                    (let-cont (cdr ids) rest-exp body parent-cont))])]
    [(op-cont name exits-exp-val exps parent-cont)
     (match exps
       [(list ) (apply-cont parent-cont (value-of-op name (reverse (cons exp-val-applied exits-exp-val))) env-applied)]
       [(list first-exp rest-exp ...) (value-of/k first-exp env-applied (op-cont name (cons exp-val-applied exits-exp-val) rest-exp parent-cont))])]))


(: value-of/k (-> expression env cont ExpVal))
(define (value-of/k expression-applied env-context cont-context)
  (match expression-applied
    [(ast-number n) (apply-cont cont-context n env-context)]
    [(ast-identifier id) (apply-cont cont-context (apply-env env-context id) env-context)]
    [(ast-if cond-exp true-exp false-exp) (value-of/k cond-exp env-context (if-cont true-exp false-exp cont-context))]
    [(ast-let id-exp-list body)
     (match id-exp-list
       [(list (cons (ast-identifier first-id) first-exp) rest-id-exp-list ...)
        (value-of/k first-exp env-context
                    (let-cont
                     (map (lambda ([x : (Pairof ast-identifier expression)]) (ast-identifier-symbol (car x))) id-exp-list)
                     (map (lambda ([x : (Pairof ast-identifier expression)]) (cdr x)) rest-id-exp-list)
                     body cont-context))])]
    [(ast-operation id params)
     (match params
       [(list ) (value-of-op id (list ))]
       [(list first-param rest-params ...) (value-of/k first-param env-context (op-cont id (list ) rest-params cont-context))])]))

(: run (-> String ExpVal))
(define (run source-code) (value-of/k (parse source-code) (empty-env) (end-cont)))

(provide run)
