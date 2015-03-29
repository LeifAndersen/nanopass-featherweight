#lang racket

(require "../main.rkt"
         rackunit)

(define-language L0
  #:terminals ((bool? (b)))
  (Expr
   #:alts (e)
   b
   (when e_1 e_2)
   (if e_1 e_2 e_3)))

(define-language L1
  #:extends L0
  (Expr
   (- (when e_1 e_2))))

(define-pass pass : L0 -> L1
  (Expr : Expr -> Expr
   [(L0:Expr:when e_1 e_2)
    (L1:Expr:if e_1 e_2 #f)]))

(check-equal?
 (pass (L0:Expr:if #f #t #f))
 (L1:Expr:if #f #t #f))

(check-equal?
 (pass (L0:Expr:when #f #t))
 (L1:Expr:if #f #t #f))

(check-equal?
 (pass (L0:Expr:b #t))
 (L1:Expr:b #t))