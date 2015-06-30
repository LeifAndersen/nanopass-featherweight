#lang racket

; Test demonstrating how to build passes.
; Does when elimination.

(require "../main.rkt"
         rackunit)

; Language L0, consists of bools and if.
(define-language L0
  #:terminals ((boolean? (b)))
  (Expr (e)
   b
   (when e e)
   (if e e e)))

; Language L1, remove when clause.
; (Consists of bool and if).
(define-extended-language L1 L0
  (Expr (e)
   #:- (when e e)))

; Remove one when blocks.
(define-pass pass : L0 -> L1
  (Expr : Expr -> Expr
   [(term (L0 Expr) (when ,e1 ,e2))
    (term (L0 Expr)
          (if ,(pass:Expr e1)
              ,(pass:Expr e2)
              ,#f))]))

; ---------------------------------------------------
; Unit tests

(check-equal?
 (pass (L0:Expr:if (L0:Expr:b #f)
                   (L0:Expr:b #t)
                   (L0:Expr:b #f)))
 (L1:Expr:if (L1:Expr:b #f)
             (L1:Expr:b #t)
             (L1:Expr:b #f)))

(check-equal?
 (pass (L0:Expr:when (L0:Expr:b #f)
                     (L0:Expr:b #t)))
 (L1:Expr:if (L1:Expr:b #f)
             (L1:Expr:b #t)
             (L1:Expr:b #f)))

(check-equal?
 (pass (L0:Expr:b #t))
 (L1:Expr:b #t))
