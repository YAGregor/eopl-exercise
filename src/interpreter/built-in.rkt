#lang typed/racket
(require (only-in "ast-element.rkt" Expression))


(struct ref ([n : Integer]))

(struct exp-list ())
(struct exp-empty-list exp-list () #:transparent)
(struct exp-list-pair exp-list ([value : ExpVal] [next : exp-list]) #:transparent)

(struct exp-pair ([left : ExpVal] [right : ExpVal]))
(struct exp-array ([refs : ref]) #:mutable)

(struct exp-procedure ([param-list : (Listof Symbol)] [body : Expression] [bind-env : Env]))
(define-type ExpVal (U String Number Boolean exp-procedure exp-list exp-pair exp-array ref))

(struct empty-env ())
(struct extend-env ([parent : Env] [id : Symbol] [value : ref]) #:transparent)
(struct extend-env-rec
  ([parent : Env] [name-param-exp-list : (Listof name-param-exp)]) #:transparent)
(struct name-param-exp ([name : Symbol] [param : Symbol] [bind-exp : Expression]))

(define-type Env (U empty-env extend-env extend-env-rec))

(provide (all-defined-out))
