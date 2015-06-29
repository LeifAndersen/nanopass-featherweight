#lang racket

;; Simple test demonstrating how to make and extend a language.

(require "../main.rkt")

; Initial language
(define-language L0
  #:terminals ((number? (x))
               (symbol? (v)))
  (Expr (e)
        x
        (+ x x)
        (let ([v e] ...)
     e)))

; Extended language
(define-extended-language L1 L0
  #:terminals (#:+ (boolean? (y))
                   #:- (symbol? (v))
                   #:+ (identifier? (i)))
  (Expr (w)
        #:+ y
        #:+ (and w e)))