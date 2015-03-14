#lang racket

(require "../main.rkt")

(define-language L0
  #:terminal ((symbol? (x)))
  (Expr
    #:alts (e)
    (if e1 e2 e3)
    (if e1 e2)))
