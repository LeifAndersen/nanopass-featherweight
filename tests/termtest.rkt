#lang racket

(require "../main.rkt")

(define-language L0
  #:terminals ((symbol? (x)))
  (Expr (e)
        x))

(define x (term (L0 Expr) ,#{'hello : symbol?}))
