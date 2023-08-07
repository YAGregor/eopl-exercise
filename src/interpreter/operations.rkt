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
    [(list  v1 v2) (eopl-pare v1 v2)]))

(define (op-list params)
  (match params
    [null (eopl-empty-list )]
    [(list head rest ...) (eopl-pare head (op-list rest))]))

(define (op-car params)
  (match params
    [(list (eopl-pare head rest) ) head]))

(define (op-cdr params)
  (match params
    [(list (eopl-pare head rest)) rest]))

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

(define (value-of-op op-name params)
  (match op-name
    ['zero? (op-zero? params)]
    ['minus (op-minus params)]
    ['equal? (op-equals? params)]
    ['greater? (op-greater? params)]
    ['less? (op-less? params)]
    ['+ (op-+ params)]
    ['- (op-- params)]
    ['* (op-* params)]
    ['/ (op-/ params)]
    ['cons (op-cons params)]
    ['list (op-list params)]
    ['car (op-car params)]
    ['cdr (op-cdr params)]
    ['newref (op-newref params)]
    ['deref (op-deref params)]
    ['setref (op-setref params)]))

(provide op-zero? op-minus op-equals? op-greater? op-less? op-+ op-- op-* op-/ op-cons op-list op-car op-cdr value-of-op)
