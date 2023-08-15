#lang typed/racket
(require (only-in "built-in.rkt" ExpVal ref))

(define #{the-store : (Listof ExpVal)} null)

(: initialize-the-store! (-> Void))
(define (initialize-the-store!) (set! the-store null))

(: get-store (-> (Listof ExpVal)))
(define (get-store ) the-store)

(: newref (-> ExpVal ref))
(define (newref value)
  (let ([ref-no (length the-store)])
    (set! the-store (append the-store (list value)))
    (ref ref-no)))

(: deref (-> ref ExpVal))
(define (deref r)
  (match r
    [(ref n) (list-ref the-store n)]))

(: setref! (-> ref ExpVal Void))
(define (setref! r v)
  (match r
    [(ref r-n)
     (letrec ([#{set-ref-inner : (-> Number (Listof ExpVal) (Listof ExpVal))}
               (lambda ([n : Number] [store : (Listof ExpVal)])
                 (match store
                   [(list curr rest ...)
                    (cond
                      [(eq? n 0) (cons v rest)]
                      [else (cons curr (set-ref-inner (- n 1) rest))])]))])
       (set! the-store (set-ref-inner r-n the-store)))]))

(provide initialize-the-store! get-store newref deref setref!)
