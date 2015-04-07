#lang racket

;; Simple test demonstrating how to make and extend a language.

(require "../main.rkt")

; Initial language
(define-language L0
  #:terminals ((number? (x))
               (symbol? (v)))
  (Expr
   #:alts (e)
   x
   (+ x_1 x_2)
   (let ([v e_1] ...)
     e_2)))

; Extended language
(define-language L1
  #:extends L0
  #:+terminals ((boolean? (y)))
  (Expr
   #:alts (w)
   (+ y
      (and w_1 e_2)))))
