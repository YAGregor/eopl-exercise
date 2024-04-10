#lang typed/racket

(require (prefix-in out: "cps-out-struct.rkt")
         (prefix-in in: "sytanx-struct.rkt"))

(: fresh-identifier (-> Symbol))
(define fresh-identifier
  (let ([i 0])
    (lambda ()
      (let ([result (string->symbol (format "gensym-~a" i))])
        (set! i (+ i 1))
        result))))

(: inp-exp-simple? (-> in:Expression Boolean))
(define (inp-exp-simple? expression)
  (match expression
    [(? number?) true]
    [(? symbol?) true]
    [(in:diff-exp exp1 exp2)
     (and (inp-exp-simple? exp1)
          (inp-exp-simple? exp2))]
    [(in:zero?-exp exp1) (inp-exp-simple? exp1)]
    [(? in:proc-exp?) true]
    [_ false]))

(define especial-k-param 'gensym-k)

(: cps-of-simple-exp (-> in:Expression out:Simple-Exp))
(define (cps-of-simple-exp expression)
  (match expression
    [(? number?) expression]
    [(? symbol?) expression]
    [(in:diff-exp e1 e2)
     (out:diff-exp (cps-of-simple-exp e1)
                   (cps-of-simple-exp e2))]
    [(in:zero?-exp e1) (out:zero?-exp (cps-of-simple-exp e1))]
    [(in:proc-exp params body)
     (out:proc-exp (append params (list especial-k-param))
                   (cps-of-exp body especial-k-param))]))

(: cps-of-exps (-> (Listof in:Expression)
                   (-> (Listof out:Simple-Exp) out:Expression)
                   out:Expression))
(define (cps-of-exps exps builder)
  (let cps-of-rest ((exps exps))
    (let ([complex-pos
           (index-where exps
                        (lambda ([e : in:Expression])
                          (not (inp-exp-simple? e))))])
      (match complex-pos
        [#f (builder (map cps-of-simple-exp exps))]
        [(? number?) (let ([var (fresh-identifier )])
                       (cps-of-exp (list-ref exps complex-pos)
                                   (out:proc-exp (list var)
                                                 (cps-of-rest (list-set exps complex-pos var)))))]))))

(: make-send-to-cont (-> out:Simple-Exp out:Simple-Exp out:Expression))
(define (make-send-to-cont k-exp simple-exp)
  (out:call-exp k-exp (list simple-exp)))

(: cps-of-zero?-exp (-> in:zero?-exp out:Simple-Exp out:Expression))
(define (cps-of-zero?-exp zero-exp k-exp)
  (match zero-exp
    [(in:zero?-exp e1)
     (cps-of-exps (list e1)
                  (lambda (exp-list)
                    (make-send-to-cont k-exp (out:zero?-exp (car exp-list)))))]))

(: cps-of-diff-exp (-> in:diff-exp out:Simple-Exp out:Expression))
(define (cps-of-diff-exp diff-exp k-exp)
  (match diff-exp
    [(in:diff-exp e1 e2)
     (cps-of-exps (list e1 e2)
                  (lambda (exp-list)
                    (match exp-list
                      [(list e1 e2) (make-send-to-cont k-exp (out:diff-exp e1 e2))])))]))

(: cps-of-if-exp (-> in:if-exp out:Simple-Exp out:Expression))
(define (cps-of-if-exp if-exp k-exp)
  (match if-exp
    [(in:if-exp cond-e e1 e2)
     (cps-of-exps (list cond-e)
                  (lambda (exp-list)
                    (match exp-list
                      [(list cond-e)
                       (out:if-exp cond-e
                                   (cps-of-exp e1 k-exp)
                                   (cps-of-exp e2 k-exp))])))]))

(: cps-of-let-exp (-> in:let-exp out:Simple-Exp out:Expression))
(define (cps-of-let-exp let-exp k-exp)
  (match let-exp
    [(in:let-exp id bind-exp body)
     (cps-of-exps (list bind-exp)
                  (lambda (exp-list)
                    (match exp-list
                      [(list exp1)
                       (out:let-exp id
                                    exp1
                                    (cps-of-exp body k-exp))])))]))

(: cps-of-call-exp (-> in:call-exp out:Simple-Exp out:Expression))
(define (cps-of-call-exp call-exp k-exp)
  (match call-exp
    [(in:call-exp rator rands)
     (cps-of-exps (cons rator rands)
                  (lambda (exp-list)
                    (match exp-list
                      [(list rator rands ...)
                       (out:call-exp rator
                                     (append rands
                                             (list k-exp)))])))]))

(: cps-of-let-rec-exp (-> in:let-rec-exp out:Simple-Exp out:Expression))
(define (cps-of-let-rec-exp rec-exp k-exp)
  (match rec-exp
    [(in:let-rec-exp id params body in)
     (out:let-rec-exp id
                      (append params (list especial-k-param))
                      (cps-of-exp body especial-k-param)
                      (cps-of-exp in k-exp))]))

(: cps-of-exp (-> in:Expression out:Simple-Exp out:Expression))
(define (cps-of-exp in-exp k-exp)
  (match in-exp
    [(? number?) (make-send-to-cont k-exp in-exp)]
    [(? symbol?) (make-send-to-cont k-exp in-exp)]
    [(in:proc-exp params body)
     (make-send-to-cont k-exp
                        (out:proc-exp (append params (list especial-k-param))
                                      (cps-of-exp body especial-k-param)))]
    [(? in:zero?-exp?) (cps-of-zero?-exp in-exp k-exp)]
    [(? in:diff-exp?) (cps-of-diff-exp in-exp k-exp)]
    [(? in:if-exp?) (cps-of-if-exp in-exp k-exp)]
    [(? in:let-exp?) (cps-of-let-exp in-exp k-exp)]
    [(? in:let-rec-exp?) (cps-of-let-rec-exp in-exp k-exp)]
    [(? in:call-exp?) (cps-of-call-exp in-exp k-exp)]))

(provide cps-of-exp)
