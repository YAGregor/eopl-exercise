#lang typed/racket

(require "ast-element.rkt" "typed-parser.rkt" "typed-ops.rkt" "state.rkt"
         (only-in "built-in.rkt" env extend-env extend-env-rec empty-env name-param-exp name-param-exp-name exp-procedure exp-procedure? ExpVal ref))

(struct end-cont ())
(struct if-cont ([true-exp : Expression] [false-exp : Expression] [parent-cont : cont]))
(struct let-cont ([identifiers : (Listof Symbol)] [exps : (Listof Expression)] [body-exp : Expression] [parent-cont : cont]))
(struct op-cont ([name : Symbol] [exist-exp-val : (Listof ExpVal)] [exps : (Listof Expression)] [parent-cont : cont]))
(struct rator-cont ([rands : (Listof Expression)] [parent-cont : cont]))
(struct rand-cont ([proc : exp-procedure] [param-exp-vals : (Listof ExpVal)] [param-exps : (Listof Expression)] [parent-cont : cont]))
(struct begin-cont ([exps : (Listof Expression)] [parent-cont : cont]))
(struct assign-cont  ([id : Symbol] [parent-cont : cont]))

(: apply-env (-> env Symbol ref))
(define (apply-env applied-env id)
  (match applied-env
    [(extend-env parent extend-id value)
     (cond [(equal? id extend-id) value]
           [else (apply-env parent id)])]
    [(extend-env-rec parent name-param-exp-list)
     (match (findf (compose (curry equal? id) name-param-exp-name) name-param-exp-list)
       [(name-param-exp n p exp) (newref (exp-procedure (list p) exp applied-env))]
       [_ (apply-env parent id)])]))


(: register-val (Union Void ExpVal))
(define register-val (void))

(: register-env (Union Void env))
(define register-env (void))

(: register-cont (Union Void cont))
(define register-cont (void))

(: register-expression (Union Void Expression))
(define register-epxression (void))

(: resgister-proc (Union Void exp-procedure))
(define register-proc (void))

(: register-params (Union Void (Listof ExpVal)))
(define register-params (void))

(define (init-register )
  (set! register-val (void))
  (set! register-env empty-env)
  (set! register-cont (end-cont))
  (set! register-epxression (void))
  (set! resgister-proc (void))
  (set! register-params (void)))


(: apply-cont (-> ExpVal))
(define (apply-cont )
  (match register-cont
    [(? end-cont?) (begin (printf "end of computation ~s \n" register-val) register-val)]
    [(if-cont true-exp false-exp parent-cont)
     (cond [register-val
            (begin
              (set! register-epxression true-exp)
              (set! register-cont parent-cont)
              (value-of/k))]
           [else
            (begin
              (set! register-epxression false-exp)
              (set! register-cont parent-cont)
              (value-of/k))])]
    [(let-cont ids exps body parent-cont)
     (match exps
       [(list )
        (begin
          (set! register-env (extend-env register-env (car ids) (newref register-val)))
          (set! register-cont parent-cont)
          (value-of/k))]
       [(list first-exp rest-exp ...)
        (begin
          (set! register-epxression first-exp)
          (set! register-env (extend-env register-env (car ids) (newref register-val)))
          (set! register-cont (let-cont (cdr ids) rest-exp body parent-cont))
          (value-of/k))])]
    [(op-cont name exits-exp-val exps parent-cont)
     (match exps
       [(list )
        (begin
          (set! register-cont parent-cont)
          (set! register-val (value-of-op name (reverse (cons register-val exits-exp-val))))
          (apply-cont))]
       [(list first-exp rest-exp ...)
        (begin
          (set! register-epxression first-exp)
          (set! register-cont (op-cont name (cons register-val exits-exp-val) rest-exp parent-cont))
          (value-of/k))])]
    [(rator-cont rands parent-cont)
     (match register-val
       [(exp-procedure param-list body proc-env)
        (match rands
          [(list )
           (begin
             (set! register-cont parent-cont)
             (set! register-proc register-val)
             (set! register-params (list ))
             (value-of-proc-call/k))]
          [(list first-exp rest-exp ...)
           (begin
             (set! register-epxression first-exp)
             (set! register-cont (rand-cont register-val (list ) rest-exp parent-cont ))
             (value-of/k))])])]
    [(rand-cont proc param-exp-vals param-exps parent-cont)
     (match param-exps
       [(list )
        (begin
          (set! register-proc proc)
          (set! register-params (reverse (cons register-val param-exp-vals)))
          (set! register-cont parent-cont)
          (value-of-proc-call/k))]
       [(list first-param rest-params ...)
        (begin
          (set! register-epxression first-param)
          (set! register-cont ( rand-cont proc (cons register-val param-exp-vals) rest-params parent-cont ))
          (value-of/k))])]
    [(begin-cont exps parent-cont)
     (match exps
       [(list )
        (begin
          (set! register-cont parent-cont)
          (apply-cont))]
       [(list first-exp rest-exps ...)
        (begin
          (set! register-expression first-exp)
          (set! register-cont (begin-cont rest-exps parent-cont))
          (value-of/k))])]
    [(assign-cont id parent-cont)
     (begin
       (setref! (apply-env env-applied id) exp-val-applied)
       (set! register-cont parent-cont)
       (set! register-val 1)
       (apply-cont))]))

(: value-of-proc-call/k (-> ExpVal))
(define (value-of-proc-call/k)
  (match proc
    [(exp-procedure param-exps body proc-env)
     (value-of/k cond-exp env-context (if-cont true-exp false-exp cont-context))
     (begin
       (set! register-epxression body)
       (set! register-env
             (foldl (lambda ([id : Symbol] [exp-val : ExpVal] [pre-env : env] ) (extend-env pre-env id (newref exp-val))) proc-env param-exps register-params))
       (value-of/k))]))

(: value-of/k (->  ExpVal))
(define (value-of/k)
  (match expression-applied
    [(ast-number n)
     (begin
       (set! register-val n)
       (apply-cont))]
    [(ast-identifier id)
     (begin
       (set! register-cont (deref (apply-env register-env id)))
       (apply-cont))]
    [(ast-proc param-exps body )
     (begin
       (set! register-val (exp-procedure (map ast-identifier-symbol param-exps) body register-env))
       (apply-cont))]
    [(ast-if cond-exp true-exp false-exp)
     (begin
       (set! register-epxression cond-exp)
       (set! register-cont (if-cont true-exp false-exp register-cont))
       (value-of/k))]
    [(ast-let id-exp-list body)
     (match id-exp-list
       [(list (cons (ast-identifier first-id) first-exp) rest-id-exp-list ...)
        (begin
          (set! register-epxression first-exp)
          (set! register-cont (let-cont
                               (map (lambda ([x : (Pairof ast-identifier expression)]) (ast-identifier-symbol (car x))) id-exp-list)
                               (map (lambda ([x : (Pairof ast-identifier expression)]) (cdr x)) rest-id-exp-list)
                               body register-cont))
          (value-of/k))])
     ]
    [(ast-let-rec name-param-exp-list body)
     (begin
       (set! register-epxression body)
       (set! register-env
             (extend-env-rec
              register-env
              (map (lambda (x)
                     (match x
                       [(ast-name-param-exp
                         (ast-identifier n)
                         (ast-identifier p)
                         e)
                        (name-param-exp n p e)]))
                   name-param-exp-list)))
       (value-of/k))]
    [(ast-proc-call proc-exp param-exp-list)
     (begin
       (set! register-epxression proc-exp)
       (set! register-cont (rator-cont param-exp-list cont-context))
       (value-of/k))]
    [(ast-operation id params)
     (match params
       [(list )
        (value-of-op id (list ))]
       [(list first-param rest-params ...)
        (begin
          (set! register-epxression first-param)
          (set! register-cont  (op-cont id (list ) rest-params cont-context))
          (value-of/k))])]
    [(ast-begin exps)
     (match exps
       [(list first-exp rest-exp ...)
        (begin
          (set! register-epxression first-exp)
          (set! register-cont (begin-cont rest-exp cont-context))
          (value-of/k))])]
    [(ast-assign (ast-identifier id) assign-exp)
     (begin
     (set! register-epxression assign-exp)
     (set! register-cont  (assign-cont id cont-context))
     (value-of/k))]))



(: run (-> String ExpVal))
(define (run source-code)
  (init-register )
  (set! register-epxression (parse source-code))
  (value-of/k))

(provide run)
