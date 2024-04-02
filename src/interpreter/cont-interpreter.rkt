#lang typed/racket

(require "ast-element.rkt" "typed-parser.rkt" "typed-ops.rkt" "state.rkt"
         (only-in "built-in.rkt" Env extend-env extend-env-rec empty-env name-param-exp name-param-exp-name exp-procedure exp-procedure? ExpVal ref))

(struct end-cont ())
(struct if-cont ([true-exp : Expression] [false-exp : Expression]  [env : Env] [parent-cont : Cont]))
(struct let-cont ([identifiers : (Listof Symbol)] [exps : (Listof Expression)] [body-exp : Expression] [env : Env] [parent-cont : Cont]))
(struct op-cont ([name : Symbol] [exist-exp-val : (Listof ExpVal)] [exps : (Listof Expression)] [env : Env] [parent-cont : Cont]))
(struct rator-cont ([rands : (Listof Expression)] [env : Env] [parent-cont : Cont]))
(struct rand-cont ([proc : exp-procedure] [param-exp-vals : (Listof ExpVal)] [param-exps : (Listof Expression)] [env : Env] [parent-cont : Cont]))
(struct begin-cont ([exps : (Listof Expression)] [env : Env] [parent-cont : Cont]))
(struct assign-cont  ([id : Symbol] [env : Env] [parent-cont : Cont]))
(struct raise-cont ([parent-cont : Cont]))
(struct try-cont ([catch-expression : Expression] [catch-id : Symbol] [parent-env : Env] [parent-cont : Cont]))

(define-type Cont (U end-cont if-cont let-cont op-cont rator-cont rand-cont begin-cont assign-cont raise-cont try-cont))

(: apply-env (-> Env Symbol ref))
(define (apply-env applied-env id)
  (match applied-env
    [(extend-env parent extend-id value)
     (cond [(equal? id extend-id) value]
           [else (apply-env parent id)])]
    [(extend-env-rec parent name-param-exp-list)
     (match (findf (compose (curry equal? id) name-param-exp-name) name-param-exp-list)
       [(name-param-exp n p exp) (newref (exp-procedure (list p) exp applied-env))]
       [_ (apply-env parent id)])]))

(: apply-cont (-> ExpVal Cont ExpVal))
(define (apply-cont exp-val cont)
  (match cont
    [(? end-cont?) (begin (printf "end of computation ~s \n" exp-val) exp-val)]
    [(if-cont true-exp false-exp env parent-cont)
     (cond [exp-val
            (value-of/k true-exp env parent-cont)]
           [else
            (value-of/k false-exp env parent-cont)])]
    [(let-cont ids exps body env parent-cont)
     (match exps
       [(list )
        (value-of/k body (extend-env env (car ids) (newref exp-val)) parent-cont)]
       [(list first-exp rest-exp ...)
        (value-of/k first-exp
                    env
                    (let-cont (cdr ids)
                              rest-exp
                              body
                              (extend-env env (car ids) (newref exp-val)) parent-cont))])]
    [(op-cont name exits-exp-val exps env parent-cont)
     (match exps
       [(list )
        (apply-cont (value-of-op name (reverse (cons exp-val exits-exp-val))) parent-cont)]
       [(list first-exp rest-exp ...)
        (value-of/k first-exp
                    env
                    (op-cont name
                             (cons exp-val exits-exp-val)
                             rest-exp
                             env
                             parent-cont))])]
    [(rator-cont rands env parent-cont)
     (match exp-val
       [(exp-procedure param-list body proc-env)
        (match rands
          [(list )
           (value-of-proc-call/k (cast exp-val exp-procedure)
                                 (list )
                                 parent-cont)]
          [(list first-exp rest-exp ...)
           (value-of/k first-exp
                       env
                       (rand-cont (cast exp-val exp-procedure)
                                  (list )
                                  rest-exp
                                  env
                                  parent-cont))])])]
    [(rand-cont proc param-exp-vals param-exps env parent-cont)
     (match param-exps
       [(list )
        (value-of-proc-call/k proc
                              (reverse (cons exp-val param-exp-vals))
                              parent-cont)]
       [(list first-param rest-params ...)
        (value-of/k first-param
                    env
                    (rand-cont proc
                               (cons exp-val param-exp-vals)
                               rest-params
                               env
                               parent-cont))])]
    [(begin-cont exps env parent-cont)
     (match exps
       [(list )
        (apply-cont exp-val parent-cont)]
       [(list first-exp rest-exps ...)
        (value-of/k first-exp
                    env
                    (begin-cont rest-exps env parent-cont))])]
    [(assign-cont id env parent-cont)
     (begin
       (setref! (apply-env env id) exp-val)
       (apply-cont 1 parent-cont))]
    [(raise-cont parent-cont) (apply-raise exp-val parent-cont)]
    [(try-cont raise-brach bind-id env parent-cont) (apply-cont exp-val parent-cont)]))

(: apply-raise (-> ExpVal Cont ExpVal))
(define (apply-raise exp-val cont)
  (match cont
    [(? end-cont?) (error "error raise reach end cont")]
    [(? if-cont?)
     (apply-raise exp-val (if-cont-parent-cont cont))]
    [(? let-cont?)
     (apply-raise exp-val (let-cont-parent-cont cont))]
    [(? op-cont?)
     (apply-raise exp-val (op-cont-parent-cont cont))]
    [(? rator-cont?)
     (apply-raise exp-val (rator-cont-parent-cont cont))]
    [(? rand-cont?)
     (apply-raise exp-val (rand-cont-parent-cont cont))]
    [(? begin-cont?)
     (apply-raise exp-val (begin-cont-parent-cont cont))]
    [(? assign-cont?) (apply-cont exp-val (assign-cont-parent-cont cont))]
    [(? raise-cont?) (apply-cont exp-val (raise-cont-parent-cont cont))]
    [(try-cont catch-exp catch-id parent-env parent-cont)
     (value-of/k catch-exp
                 (extend-env parent-env catch-id (newref exp-val))
                 parent-cont)]))

(: value-of-proc-call/k (-> exp-procedure (Listof ExpVal) Cont ExpVal))
(define (value-of-proc-call/k proc param-vals cont)
  (match proc
    [(exp-procedure param-exps body proc-env)
     (value-of/k body
                 (foldl (lambda ([id : Symbol] [exp-val : ExpVal] [pre-env : Env] )
                          (extend-env pre-env id (newref exp-val)))
                        proc-env
                        param-exps
                        param-vals)
                 cont)]))

(: value-of/k (-> Expression Env Cont ExpVal))
(define (value-of/k expression env cont)
  (match expression
    [(ast-number n)
     (apply-cont n cont)]
    [(ast-string s)
     (apply-cont s cont)]
    [(ast-identifier id)
     (let ([value (deref (apply-env env id))])
       (apply-cont value cont))]
    [(ast-proc param-exps body)
     (apply-cont
      (exp-procedure (map ast-identifier-symbol param-exps) body env)
      cont)]
    [(ast-if cond-exp true-exp false-exp)
     (value-of/k cond-exp env (if-cont true-exp false-exp env cont))]
    [(ast-let id-exp-list body)
     (match id-exp-list
       [(list (cons (ast-identifier first-id) first-exp) rest-id-exp-list ...)
        (value-of/k first-exp env
                    (let-cont (map (lambda ([x : (Pairof ast-identifier Expression)]) (ast-identifier-symbol (car x))) id-exp-list)
                              (map (lambda ([x : (Pairof ast-identifier Expression)]) (cdr x)) rest-id-exp-list)
                              body
                              env
                              cont))])]
    [(ast-let-rec name-param-exp-list body)
     (value-of/k body
                 (extend-env-rec
                  env
                  (map (lambda ([x : ast-name-param-exp])
                         (match x
                           [(ast-name-param-exp (ast-identifier n)
                                                (ast-identifier p)
                                                e)
                            (name-param-exp n p e)]))
                       name-param-exp-list))
                 cont)]
    [(ast-proc-call proc-exp param-exp-list)
     (value-of/k proc-exp env (rator-cont param-exp-list env cont))]
    [(ast-operation id params)
     (match params
       [(list )
        (apply-cont
         (value-of-op id (list ))
         cont)]
       [(list first-param rest-params ...)
        (value-of/k first-param
                    env
                    (op-cont id
                             (list )
                             rest-params
                             env
                             cont))])]
    [(ast-begin exps)
     (match exps
       [(list first-exp rest-exp ...)
        (value-of/k first-exp
                    env
                    (begin-cont rest-exp env cont))])]
    [(ast-assign (ast-identifier id) assign-exp)
     (value-of/k assign-exp
                 env
                 (assign-cont id env cont))]
    [(ast-raise expression)
     (value-of/k expression
                 env
                 (raise-cont cont))]
    [(ast-try try-branch (ast-identifier id-symbol) catch-branch)
     (value-of/k try-branch env (try-cont catch-branch id-symbol env cont))]))

(: run (-> String ExpVal))
(define (run source-code)
  (initialize-the-store!)
  (let ([ast : Expression (parse source-code)])
    (value-of/k ast
                (empty-env)
                (end-cont))))

(provide run)
