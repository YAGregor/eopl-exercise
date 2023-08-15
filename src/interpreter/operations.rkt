#lang racket

(require "built-in.rkt" "state.rkt")

(define invalid-params "invalid params")

(define (op-zero? params)
  (match params
    [(list (? number? n)) (= n 0)]
    [_ (error invalid-params)]))

(define (op-minus params)
  (match params
    [(list (? number? n)) (- n)]
    [_ (error invalid-params)]))

(define (op-equals? params)
  (match params
    [(list v1 v2) (equal? v1 v2)]
    [_ (error invalid-params)]))

(define (op-greater? params)
  (match params
    [(list (? number? v1) (? number? v2)) (> v1 v2)]
    [_ (error invalid-params)]))

(define (op-less? params)
  (match params
    [(list (? number? v1) (? number? v2)) (< v1 v2)]))

(define (op-+ params)
  (match params
    [(list (? number? v1) (? number? v2)) (+ v1 v2)]))

(define (op-- params)
  (match params
    [(list (? number? v1) (? number? v2)) (- v1 v2)]
    [_ (error invalid-params)]))

(define (op-* params)
  (match params
    [(list (? number? v1) (? number? v2)) (* v1 v2)]))

(define (op-/ params)
  (match params
    [(list (? number? v1) (? number? v2)) (/ v1 v2)]))

(define (op-cons params)
  (match params
    [(list  v1 v2) (exp-list-pair v1 v2)]))

(define (op-list params)
  (match params
    [(list ) (exp-empty-list )]
    [(list head rest ...) (exp-list-pair head (op-list rest))]))

(define (op-car params)
  (match params
    [(list (exp-list-pair head rest) ) head]))

(define (op-cdr params)
  (match params
    [(list (exp-list-pair head rest)) rest]))

(define (op-newref params)
  (match params
    [(list value)
     (newref value)]))

(define (op-deref params)
  (match params
    [(list ref) (deref ref)]))

(define (op-setref params)
  (match params
    [(list ref value) (setref! ref value)]))

(define (op-make-pair params)
  (match params
    [(list left right) (exp-pair (newref left) (newref right))]))

(define (op-left params)
  (match params
    [(list (exp-pair left _)) (deref left)]))

(define (op-right params)
  (match params
    [(list (exp-pair _ right)) (deref right)]))


(define (op-set-left params)
  (match params
    [(list (exp-pair left _) value) (setref! left value)]))

(define (op-set-right params)
  (match params
    [(list (exp-pair _ right) value) (setref! right value)]))

(define (op-newarray params)
  (exp-array (map newref params)))

(define (op-arrayref params)
  (match params
    [(list (exp-array ref-list) index) (deref (list-ref ref-list index))]))

(define (op-arrayset params)
  (match params
    [(list array index value)
     (begin
       (set-exp-array-refs! array (list-set (exp-array-refs array) index (newref value)))
       array)]))

(define (value-of-op op-name params)
  ((match op-name
     ['zero? op-zero?]
     ['minus op-minus]
     ['equal? op-equals?]
     ['greater? op-greater?]
     ['less? op-less?]
     ['+ op-+]
     ['- op--]
     ['* op-*]
     ['/ op-/]
     ['cons op-cons]
     ['list op-list]
     ['car op-car]
     ['cdr op-cdr]
     ['newref op-newref]
     ['deref op-deref]
     ['setref op-setref]
     ['pair op-make-pair]
     ['left op-left]
     ['right op-right]
     ['setleft op-set-left]
     ['setright op-set-right]
     ['newarray op-newarray]
     ['arrayref op-arrayref]
     ['arrayset op-arrayset]) params))

(provide op-zero? op-minus op-equals? op-greater? op-less? op-+ op-- op-* op-/ op-cons op-list op-car op-cdr value-of-op)
