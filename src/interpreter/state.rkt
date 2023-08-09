#lang racket

(define the-store null)

(struct ref (n) #:transparent)

(define (initialize-the-store!) (set! the-store null))

(define (get-store ) the-store)

(define (newref value)
  (let ([ref-no (length the-store)])
    (set! the-store (append the-store (list value)))
    (ref ref-no)))

(define (deref r)
  (match r
    [(ref n) (list-ref the-store n)]))

(define (setref! r v)
  (match r
    [(ref r-n)
     (letrec ([set-ref-inner
               (lambda (n store)
                 (match store
                   [(list curr rest ...)
                    (cond
                      [(eq? n 0) (cons v rest)]
                      [else (cons curr (set-ref-inner (- n 1) rest))])]))])
       (set! the-store (set-ref-inner r-n the-store)))]))

(provide (struct-out ref) initialize-the-store! get-store newref deref setref!)
