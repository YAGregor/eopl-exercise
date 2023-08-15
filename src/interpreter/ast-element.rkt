#lang typed/racket

(struct expression () #:transparent)

(struct ast-number expression ([n : Number]) #:transparent)
(struct ast-boolean expression ([b : Boolean]) #:transparent)
(struct ast-identifier expression ([symbol : Symbol]) #:transparent)

(struct ast-if expression ([cond : expression] [true : expression] [false : expression]) #:transparent)
(struct ast-let expression ([id-exp-list : (Listof (Pairof ast-identifier expression))] [expression : expression]) #:transparent)
(struct ast-operation expression ([name : Symbol] [parameters : (Listof expression)]) #:transparent)
(struct ast-emptylist expression () #:transparent)
(struct ast-proc expression ([identifier-list : (Listof ast-identifier)] [expression : expression]) #:transparent)
(struct ast-proc-call expression ([proc : expression] [param-list : (Listof expression)]) #:transparent)
(struct ast-name-param-exp ([name : ast-identifier] [param : expression] [exp : expression]) #:transparent)
(struct ast-let-rec expression ([name-param-exp-list : (Listof ast-name-param-exp)] [body : expression]) #:transparent)
(struct ast-begin expression ([exp-list : (Listof expression)]) #:transparent)
(struct ast-assign expression ([id : ast-identifier] [expression : expression]) #:transparent)

(provide (struct-out ast-number) (struct-out ast-boolean) (struct-out ast-identifier) (struct-out ast-if) (struct-out ast-if) (struct-out ast-let) (struct-out ast-operation)
          (struct-out ast-emptylist) (struct-out ast-proc) (struct-out ast-proc-call) (struct-out ast-let-rec) (struct-out ast-begin) (struct-out ast-name-param-exp)
          (struct-out ast-assign) (struct-out expression))
