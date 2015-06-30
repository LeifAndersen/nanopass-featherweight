#lang racket

(require "../main.rkt")

(define-language L0
  (Expr integer?
        (+ Expr Expr)))

(define x (term (L0 Expr) ,#{'hello : symbol?}))
