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
    (L1:Expr:if (pass:Expr e_1)
                (pass:Expr e_2)
                (L1:Expr:b #f))]))

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