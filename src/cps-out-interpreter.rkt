#lang typed/racket


(require "cps-out-struct.rkt" "typed-cps-out-parser.rkt")

(struct proc ([param-list : (Listof Symbol)] [body : Expression] [env : Env]))

(define-type ExpVal (U Number Boolean proc))

(struct end-cont ())
(struct diff-cont ([exp2 : Expression] [parent-cont : Cont] [env : Env]))
(struct diff-2-cont ([exp-val : Number] [parent-cont : Cont] [env : Env]))
(struct zero-cont ([parent-cont : Cont]))
(struct if-cont ([true-exp : Expression] [false-exp : Expression] [parent-cont : Cont] [env : Env]))
(struct let-cont ([id : Symbol] [in-exp : Expression] [parent-cont : Cont] [env : Env]))
(struct rator-cont ([rand-list : (Listof Expression)] [parent-cont : Cont] [env : Env]))
(struct rand-cont ([rator-exp-val : proc] [rand-exp-list : (Listof Expression)] [rand-exp-val-list : (Listof ExpVal)] [parent-cont : Cont] [env : Env]))
(define-type Cont (U end-cont diff-cont diff-2-cont zero-cont if-cont let-cont rator-cont rand-cont))

(struct empty-env ())
(struct extend-env ([parent : Env] [id : Symbol] [value : ExpVal]))
(struct extend-rec-env ([parent : Env] [id : Symbol] [param-list : (Listof Symbol)] [body : Expression]))
(define-type Env (U empty-env extend-env extend-rec-env))

(: apply-env (-> Env Symbol ExpVal))
(define (apply-env env id)
  (match env
    [(? empty-env?) (error "id not found")]
    [(extend-env parent extend-id value)
     (cond
       [(eq? id extend-id) value]
       [else (apply-env parent id)])]
    [(extend-rec-env parent-env extend-id param-list body)
     (cond
       [(eq? id extend-id) (proc param-list body env)]
       [else (apply-env parent-env id)])]))

(: apply-cont (-> Cont ExpVal ExpVal))
(define (apply-cont cont exp-val )
  (match cont
    [(? end-cont?) (begin (println (format "end of computation ~a" exp-val)) exp-val)]
    [(diff-cont exp2 parent-cont env)
     (value-of/k exp2
                 env
                 (diff-2-cont (cast exp-val Number)
                              parent-cont env))]
    [(diff-2-cont exp1 parent-cont env) (apply-cont parent-cont (- exp1 (cast exp-val Number)))]
    [(zero-cont parent-cont)
     (apply-cont parent-cont
                 (eq? exp-val 0))]
    [(if-cont true-exp false-exp parent-cont env)
     (cond [exp-val
            (value-of/k true-exp env parent-cont)]
           [else
            (value-of/k false-exp env parent-cont)])]
    [(let-cont id in-exp parent-cont env)
     (value-of/k in-exp
                 (extend-env env id exp-val)
                 parent-cont)]
    [(rator-cont rand-list parent-cont env)
     (match rand-list
       [(list first-exp rest-exp-list ...)
        (value-of/k first-exp
                    env
                    (rand-cont (cast exp-val proc) rest-exp-list (list ) parent-cont env))])]
    [(rand-cont rator rand-exp-list rand-exp-val-list parent-cont env)
     (match rand-exp-list
       [(list )
        (apply-proc-call rator
                         (reverse (cons exp-val rand-exp-val-list))
                         env parent-cont)]
       [(list first-exp rest-exp ...)
        (value-of/k first-exp env (rand-cont rator rest-exp (cons exp-val rand-exp-val-list) parent-cont env))])]))

(: extend-env-many (-> Env (Listof Symbol) (Listof ExpVal) Env))
(define (extend-env-many env id-list exp-val-list)
  (foldl (lambda ([id : Symbol]
                  [exp-val : ExpVal]
                  [parent-env : Env]) (extend-env parent-env id exp-val))
         env id-list exp-val-list))

(: apply-proc-call (-> proc (Listof ExpVal) Env Cont ExpVal))
(define (apply-proc-call rator param-exp-vals env cont)
  (match rator
    [(proc param-list body proc-parent-env)
     (value-of/k body (extend-env-many proc-parent-env param-list param-exp-vals) cont)]))

(: value-of/k (-> Expression Env Cont ExpVal))
(define (value-of/k expression env cont)
  (match expression
    [(? number?) (apply-cont cont expression)]
    [(? boolean?) (apply-cont cont expression)]
    [(? symbol?) (apply-cont cont (apply-env env expression))]
    [(diff-exp exp1 exp2)
     (value-of/k exp1 env (diff-cont exp2 cont env))]
    [(zero?-exp exp1)
     (value-of/k exp1 env (zero-cont cont))]
    [(if-exp cond-exp true-exp false-exp)
     (value-of/k cond-exp env (if-cont true-exp false-exp cont env))]
    [(let-exp id bind body)
     (value-of/k bind env (let-cont id body cont env))]
    [(let-rec-exp id params body in)
     (value-of/k in (extend-rec-env env id params body) cont)]
    [(proc-exp params body) (apply-cont cont (proc params body env))]
    [(call-exp rator rands) (value-of/k rator env (rator-cont rands cont env))]))

(: run (-> String ExpVal))
(define (run source-code)
  (value-of/k (parse source-code) (empty-env) (end-cont)))

(provide run)
