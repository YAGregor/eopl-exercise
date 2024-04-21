#lang typed/racket

(struct diff-exp ([exp1 : Simple-Exp] [exp2 : Simple-Exp]) #:transparent)
(struct zero?-exp ([exp : Simple-Exp]) #:transparent)
(struct if-exp ([cond-exp : Simple-Exp] [true-exp : Expression] [false-exp : Expression]) #:transparent)
(struct print-k-exp ([print-exp : Simple-Exp] [return-exp : Expression]) #:transparent)
(struct let-exp ([id : Symbol] [bind-exp : Expression] [body : Expression]) #:transparent)
(struct let-rec-exp ([id : Symbol] [params : (Listof Symbol)] [body : Expression] [in : Expression]) #:transparent)
(struct proc-exp ([params : (Listof Symbol)] [body : Expression]) #:transparent)
(struct call-exp ([rator : Simple-Exp] [rands : (Listof Simple-Exp)]) #:transparent)

(define-type Simple-Exp (U Symbol Number proc-exp diff-exp zero?-exp))
(define-type Expression (U Simple-Exp  if-exp let-exp let-rec-exp call-exp print-k-exp))

(provide (all-defined-out))
