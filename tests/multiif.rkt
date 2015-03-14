#lang racket

(require "../main.rkt")

(define-language L0
  #:terminals ((symbol? (x)))
  (Expr
    #:alts (e)
    (if e_1 e_2 e_3)
    (if e_1 e_2)))
