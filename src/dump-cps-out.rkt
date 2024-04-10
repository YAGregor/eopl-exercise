#lang typed/racket
(require "cps-out-struct.rkt")

(: dump-params (-> (Listof Symbol) String))
(define (dump-params params)
  (let iter ([params params] [result ""])
    (match params
      [(list ) result]
      [(list f r ...)
       (iter r
             (format "~a ~a " result (symbol->string f)))])))


(: dump-proc-exp (-> proc-exp String))
(define (dump-proc-exp expression)
  (match expression
    [(proc-exp params body)
     (let ([params-string (dump-params params)])
       (format "proc (~a) ~a"
               params-string
               (dump body)))]))

(: dump-call-exp (-> call-exp String))
(define (dump-call-exp expression)
  (match expression
    [(call-exp rator rands)
     (let ([rator-dump (dump rator)]
           [rand-dump
            (let #{iter : (-> (Listof Expression) String String)}
              ([rands rands] [result ""])
              (match rands
                [(list ) result]
                [(list f r ...) (iter r (format "~a ~a" (dump f) result))]))])
       (format "(~a ~a)" rator-dump rand-dump))]))

(: dump-let-rec-exp (-> let-rec-exp String))
(define (dump-let-rec-exp expression)
  (match expression
    [(let-rec-exp id params body in)
     (format "letrec ~a (~a) = ~a in ~a"
             id (dump-params params) (dump body) (dump in))]))

(: dump-let-exp (-> let-exp String))
(define (dump-let-exp expression)
  (match expression
    [(let-exp id bind in)
     (format "let ~a = ~a in ~a"
             id (dump bind) (dump in))]))

(: dump-zero-exp (-> zero?-exp String))
(define (dump-zero-exp expression)
  (match expression
    [(zero?-exp exp1) (format "zero?(~a)" (dump exp1))]))

(: dump-diff-exp (-> diff-exp String))
(define (dump-diff-exp expression)
  (match expression
    [(diff-exp e1 e2) (format  "diff(~a,~a)" (dump e1) (dump e2))]))

(: dump-if-exp (-> if-exp String))
(define (dump-if-exp expression)
  (match expression
    [(if-exp c-e t-e f-e)
     (format "if ~a then ~a else ~a"
             (dump c-e) (dump t-e) (dump f-e))]))

(: dump (-> Expression String))
(define (dump expression)
  (match expression
    [(? number?) (number->string expression)]
    [(? symbol?) (symbol->string expression)]
    [(? if-exp?) (dump-if-exp expression)]
    [(? proc-exp?) (dump-proc-exp expression)]
    [(? call-exp?) (dump-call-exp expression)]
    [(? let-rec-exp?) (dump-let-rec-exp expression)]
    [(? let-exp?) (dump-let-exp expression)]
    [(? zero?-exp?) (dump-zero-exp expression)]
    [(? diff-exp?) (dump-diff-exp expression)]))

(provide dump)
