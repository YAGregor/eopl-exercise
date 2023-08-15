#lang typed/racket
(require (only-in "ast-element.rkt" expression))


(struct ref ([n : Integer]))

(struct exp-list ())
(struct exp-empty-list exp-list () #:transparent)
(struct exp-list-pair exp-list ([value : ExpVal] [next : exp-list]) #:transparent)

(struct exp-pair ([left : ExpVal] [right : ExpVal]))
(struct exp-array ([refs : ref]) #:mutable)

(struct procedure ([param-list : (Listof Symbol)] [body : expression] [bind-env : env]))
(define-type ExpVal (U String Number Boolean procedure exp-list exp-pair exp-array ref))

(struct env ())
(struct empty-env env () #:transparent)
(struct extend-env env ([parent : env] [id : Symbol] [value : ExpVal]) #:transparent)
(struct extend-env-rec env
  ([parent : env] [name-param-exp-list : (Listof name-param-exp)]) #:transparent)
(struct name-param-exp ([name : Symbol] [param : Symbol] [bind-exp : expression]))

(provide (all-defined-out))
