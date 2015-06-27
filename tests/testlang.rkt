#lang racket

;; Simple test demonstrating how to make and extend a language.

(require "../main.rkt")

; Initial language
(define-language L0
  #:terminals ((number? (x))
               (symbol? (v)))
  (Expr (e)
   x
   (+ x_1 x_2)
   (let ([v e_1] ...)
     e_2)))

; Extended language
(define-extended-language L1 L0
  #:terminals (#:+ (boolean? (y)))
  (Expr (w)
   #:+ y
   #:+ (and w_1 e_2)))