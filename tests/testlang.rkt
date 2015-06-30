#lang racket

;; Simple test demonstrating how to make and extend a language.

(require "../main.rkt")

(define Number number?)
(define Symbol symbol?)
(define Boolean boolean?)

; Initial language
(define-language L0
  (Expr Number
        (+ Expr Expr)
        (let ([Symbol Expr] ...)
          Expr)))

; Extended language
(define-extended-language L1 L0
  (Expr #:+ Boolean
        #:- (+ Expr Expr)
        #:+ (and Expr Expr)))
