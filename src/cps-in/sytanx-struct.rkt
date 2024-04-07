#lang typed/racket

(struct diff-exp ([exp1 : Expression] [exp2 : Expression]) #:transparent)
(struct zero?-exp ([exp : Expression]) #:transparent)
(struct if-exp ([cond-exp : Expression] [true-exp : Expression] [false-exp : Expression]) #:transparent)
(struct let-exp ([id : Symbol] [bind-exp : Expression] [body : Expression]) #:transparent)
(struct let-rec-exp ([id : Symbol] [params : (Listof Symbol)] [body : Expression]) #:transparent)
(struct proc-exp ([params : (Listof Symbol)] [body : Expression]) #:transparent)
(struct call-exp ([rator : Expression] [rands : (Listof Expression)]) #:transparent)

(define-type Expression (U Number Boolean Symbol diff-exp zero?-exp if-exp let-exp let-rec-exp proc-exp call-exp))

(provide (all-defined-out))
