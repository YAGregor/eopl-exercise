#lang typed/racket

(require "ast-element.rkt" "typed-parser.rkt" "typed-ops.rkt"
         (only-in "built-in.rkt" env extend-env extend-env-rec empty-env name-param-exp name-param-exp-name procedure ExpVal))

(struct cont ())
(struct end-cont cont ())
(struct if-cont cont ([true-exp : expression] [false-exp : expression] [parent-cont : cont]))
(struct let-cont cont ([identifiers : (Listof Symbol)] [exps : (Listof expression)] [body-exp : expression] [parent-cont : cont]))
(struct op-cont cont ([name : Symbol] [exist-exp-val : (Listof ExpVal)] [exps : (Listof expression)] [parent-cont : cont]))
(struct rator-cont cont ([rands : (Listof expression)] [parent-cont : cont]))
(struct rand-cont cont ([proc : procedure] [param-exp-vals : (Listof ExpVal)] [param-exps : (Listof expression)] [parent-cont : cont]))

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
       [(list first-exp rest-exp ...) (value-of/k first-exp env-applied (op-cont name (cons exp-val-applied exits-exp-val) rest-exp parent-cont))])]
    [(rator-cont rands parent-cont)
     (match exp-val-applied
       [(procedure param-list body proc-env)
        (match rands
          [(list ) (value-of/k body proc-env parent-cont)]
          [(list first-exp rest-exp ...) (value-of/k first-exp env-applied (rand-cont exp-val-applied (list ) rest-exp parent-cont))])])]
    [(rand-cont (procedure param-list body procedure-env) param-exp-vals param-exps parent-cont)
     (match param-exps
       [(list )
        (value-of/k
         body
         (foldl (lambda ([id : Symbol] [exp-val : ExpVal] [pre-env : env] ) (extend-env pre-env id exp-val))
                procedure-env
                param-list (reverse (cons exp-val-applied param-exps)))
         parent-cont)]
       [(list first-param rest-param ...)
        (value-of/k first-param env-applied (rand-cont (procedure param-list body procedure-env) (cons exp-val-applied param-exp-vals) rest-param parent-cont))])]))


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
    [(ast-let-rec name-param-exp-list body)
     (value-of/k body
                 (extend-env-rec
                  env-context
                  (map (lambda (x)
                         (match x
                           [(ast-name-param-exp
                             (ast-identifier n)
                             (ast-identifier p)
                             e)
                            (name-param-exp n p e)]))
                       name-param-exp-list))
                 cont-context)]
    [(ast-proc-call proc-exp param-exp-list) (value-of/k proc-exp env-context (rator-cont param-exp-list cont-context))]
    [(ast-operation id params)
     (match params
       [(list ) (value-of-op id (list ))]
       [(list first-param rest-params ...) (value-of/k first-param env-context (op-cont id (list ) rest-params cont-context))])]))

(: run (-> String ExpVal))
(define (run source-code) (value-of/k (parse source-code) (empty-env) (end-cont)))

(provide run)
