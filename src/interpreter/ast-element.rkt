#lang typed/racket

(struct ast-number ([n : Number]) #:transparent)
(struct ast-boolean ([b : Boolean]) #:transparent)
(struct ast-identifier ([symbol : Symbol]) #:transparent)
(struct ast-string ([s : String]) #:transparent)

(struct ast-if ([cond : Expression] [true : Expression] [false : Expression]) #:transparent)
(struct ast-let ([id-exp-list : (Listof (Pairof ast-identifier Expression))] [expression : Expression]) #:transparent)
(struct ast-operation ([name : Symbol] [parameters : (Listof Expression)]) #:transparent)
(struct ast-emptylist () #:transparent)
(struct ast-proc ([identifier-list : (Listof ast-identifier)] [expression : Expression]) #:transparent)
(struct ast-proc-call ([proc : Expression] [param-list : (Listof Expression)]) #:transparent)
(struct ast-name-param-exp ([name : ast-identifier] [param : Expression] [exp : Expression]) #:transparent)
(struct ast-let-rec ([name-param-exp-list : (Listof ast-name-param-exp)] [body : Expression]) #:transparent)
(struct ast-begin ([exp-list : (Listof Expression)]) #:transparent)
(struct ast-assign ([id : ast-identifier] [expression : Expression]) #:transparent)

(define-type Expression (U ast-number ast-string ast-boolean ast-identifier ast-if ast-let ast-let ast-operation ast-emptylist ast-proc ast-proc-call ast-name-param-exp ast-let-rec ast-begin ast-assign))

(provide (struct-out ast-number) (struct-out ast-string) (struct-out ast-boolean) (struct-out ast-identifier) (struct-out ast-if) (struct-out ast-if) (struct-out ast-let) (struct-out ast-operation)
          (struct-out ast-emptylist) (struct-out ast-proc) (struct-out ast-proc-call) (struct-out ast-let-rec) (struct-out ast-begin) (struct-out ast-name-param-exp)
          (struct-out ast-assign) Expression)
