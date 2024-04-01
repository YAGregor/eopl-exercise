#lang typed/racket

(require "ast-element.rkt" "typed-parser.rkt" "typed-ops.rkt" "state.rkt"
         (only-in "built-in.rkt" Env extend-env extend-env-rec empty-env name-param-exp name-param-exp-name exp-procedure exp-procedure? ExpVal ref))

(struct end-cont ())
(struct if-cont ([true-exp : Expression] [false-exp : Expression] [parent-cont : Cont]))
(struct let-cont ([identifiers : (Listof Symbol)] [exps : (Listof Expression)] [body-exp : Expression] [parent-cont : Cont]))
(struct op-cont ([name : Symbol] [exist-exp-val : (Listof ExpVal)] [exps : (Listof Expression)] [parent-cont : Cont]))
(struct rator-cont ([rands : (Listof Expression)] [parent-cont : Cont]))
(struct rand-cont ([proc : exp-procedure] [param-exp-vals : (Listof ExpVal)] [param-exps : (Listof Expression)] [parent-cont : Cont]))
(struct begin-cont ([exps : (Listof Expression)] [parent-cont : Cont]))
(struct assign-cont  ([id : Symbol] [parent-cont : Cont]))

(define-type Cont (U end-cont if-cont let-cont op-cont rator-cont rand-cont begin-cont assign-cont))

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


(: register-val (Union Void ExpVal))
(define register-val (void))
(define (get-register-val) (cast register-val ExpVal))

(: register-env (Union Void Env))
(define register-env (void))
(define (get-register-env) (cast register-env Env))

(: register-cont (Union Void Cont))
(define register-cont (void))
(define (get-register-cont) (cast register-cont Cont))


(: register-expression (Union Void Expression))
(define register-expression (void))
(define (get-register-epxression) (cast register-expression Expression))

(: register-proc (Union Void exp-procedure))
(define register-proc (void))
(define (get-register-proc) (cast register-proc exp-procedure))

(: register-params (Union Void (Listof ExpVal)))
(define register-params (void))
(define (get-register-params) (cast register-params (Listof ExpVal)))

(define (init-register )
  (set! register-val (void))
  (set! register-env (empty-env))
  (set! register-cont (end-cont))
  (set! register-expression (void))
  (set! register-proc (void))
  (set! register-params (void)))


(: apply-cont (-> ExpVal))
(define (apply-cont )
  (match register-cont
    [(? end-cont?) (begin (printf "end of computation ~s \n" register-val) (get-register-val))]
    [(if-cont true-exp false-exp parent-cont)
     (cond [register-val
            (begin
              (set! register-expression true-exp)
              (set! register-cont parent-cont)
              (value-of/k))]
           [else
            (begin
              (set! register-expression false-exp)
              (set! register-cont parent-cont)
              (value-of/k))])]
    [(let-cont ids exps body parent-cont)
     (match exps
       [(list )
        (begin
          (set! register-env (extend-env (get-register-env) (car ids) (newref (get-register-val))))
          (set! register-cont parent-cont)
          (set! register-expression body)
          (value-of/k))]
       [(list first-exp rest-exp ...)
        (begin
          (set! register-expression first-exp)
          (set! register-env (extend-env (get-register-env) (car ids) (newref (get-register-val))))
          (set! register-cont (let-cont (cdr ids) rest-exp body parent-cont))
          (value-of/k))])]
    [(op-cont name exits-exp-val exps parent-cont)
     (match exps
       [(list )
        (begin
          (set! register-cont parent-cont)
          (set! register-val (value-of-op name (reverse (cons (get-register-val) exits-exp-val))))
          (apply-cont))]
       [(list first-exp rest-exp ...)
        (begin
          (set! register-expression first-exp)
          (set! register-cont (op-cont name (cons (get-register-val) exits-exp-val) rest-exp parent-cont))
          (value-of/k))])]
    [(rator-cont rands parent-cont)
     (match register-val
       [(exp-procedure param-list body proc-env)
        (match rands
          [(list )
           (begin
             (set! register-cont parent-cont)
             (set! register-proc (cast (get-register-val) exp-procedure))
             (set! register-params (list ))
             (value-of-proc-call/k))]
          [(list first-exp rest-exp ...)
           (begin
             (set! register-expression first-exp)
             (set! register-cont (rand-cont (cast (get-register-val) exp-procedure) (list ) rest-exp parent-cont))
             (value-of/k))])])]
    [(rand-cont proc param-exp-vals param-exps parent-cont)
     (match param-exps
       [(list )
        (begin
          (set! register-proc proc)
          (set! register-params (reverse (cons (get-register-val) param-exp-vals)))
          (set! register-cont parent-cont)
          (value-of-proc-call/k))]
       [(list first-param rest-params ...)
        (begin
          (set! register-expression first-param)
          (set! register-cont ( rand-cont proc (cons (get-register-val) param-exp-vals) rest-params parent-cont ))
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
       (setref! (apply-env (get-register-env) id) (get-register-val))
       (set! register-cont parent-cont)
       (set! register-val 1)
       (apply-cont))]))

(: value-of-proc-call/k (-> ExpVal))
(define (value-of-proc-call/k)
  (match register-proc
    [(exp-procedure param-exps body proc-env)
     (begin
       (set! register-expression body)
       (set! register-env
             (foldl (lambda ([id : Symbol] [exp-val : ExpVal] [pre-env : Env] ) (extend-env pre-env id (newref exp-val))) proc-env param-exps (get-register-params)))
       (value-of/k))]))

(: value-of/k (->  ExpVal))
(define (value-of/k)
  (match register-expression
    [(ast-number n)
     (begin
       (set! register-val n)
       (apply-cont))]
    [(ast-string s)
     (begin (set! register-val s)
            (apply-cont))]
    [(ast-identifier id)
     (begin
       (set! register-val (deref (apply-env (get-register-env) id)))
       (apply-cont))]
    [(ast-proc param-exps body )
     (begin
       (set! register-val (exp-procedure (map ast-identifier-symbol param-exps) body (get-register-env)))
       (apply-cont))]
    [(ast-if cond-exp true-exp false-exp)
     (begin
       (set! register-expression cond-exp)
       (set! register-cont (if-cont true-exp false-exp (get-register-cont)))
       (value-of/k))]
    [(ast-let id-exp-list body)
     (match id-exp-list
       [(list (cons (ast-identifier first-id) first-exp) rest-id-exp-list ...)
        (begin
          (set! register-expression first-exp)
          (set! register-cont (let-cont
                               (map (lambda ([x : (Pairof ast-identifier Expression)]) (ast-identifier-symbol (car x))) id-exp-list)
                               (map (lambda ([x : (Pairof ast-identifier Expression)]) (cdr x)) rest-id-exp-list)
                               body (get-register-cont)))
          (value-of/k))])
     ]
    [(ast-let-rec name-param-exp-list body)
     (begin
       (set! register-expression body)
       (set! register-env
             (extend-env-rec
              (get-register-env)
              (map (lambda ([x : ast-name-param-exp])
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
       (set! register-expression proc-exp)
       (set! register-cont (rator-cont param-exp-list (get-register-cont)))
       (value-of/k))]
    [(ast-operation id params)
     (match params
       [(list )
        (value-of-op id (list ))]
       [(list first-param rest-params ...)
        (begin
          (set! register-expression first-param)
          (set! register-cont  (op-cont id (list ) rest-params (get-register-cont)))
          (value-of/k))])]
    [(ast-begin exps)
     (match exps
       [(list first-exp rest-exp ...)
        (begin
          (set! register-expression first-exp)
          (set! register-cont (begin-cont rest-exp (get-register-cont)))
          (value-of/k))])]
    [(ast-assign (ast-identifier id) assign-exp)
     (begin
       (set! register-expression assign-exp)
       (set! register-cont  (assign-cont id (get-register-cont)))
       (value-of/k))]))


(: run (-> String ExpVal))
(define (run source-code)
  (init-register )
  (let ([ast : Expression (parse source-code)])
    (set! register-expression ast))
  (value-of/k))

(println (run "let x = \"123\" in x"))

(provide run)
