#lang racket

(require "../main.rkt"
         rackunit)

(define-language L0
  #:terminals ((bool? (b))
               (symbol? (x)))
  (Expr
   #:alts (e)
   b
   (let ([x_1 e_1] ...) e_2)
   (let* ([x_1 e_1] ...) e_2)
   (when e_1 e_2)
   (if e_1 e_2 e_3)))

(define-language L1
  #:extends L0
  (Expr
   (- (when e_1 e_2))))

(define-language L2
  #:extends L1
  (Expr
   (- (let* ([x_1 e_1] ...) e_2))))

(define-pass remove-when : L0 -> L1
  (Expr : Expr -> Expr
   [(L0:Expr:when e_1 e_2)
    (L1:Expr:if e_1 e_2 #f)]))

(define-pass remove-let* : L1 -> L2
  (Expr : Expr -> Expr
   [(L1:Expr:let* '() '() b)
    (L2:Expr:let '() '() b)]
   [(L1:Expr:let* `(,x ,xr ...) `(,e ,er ...) b)
    (L2:Expr:let `(,x) `(,e)
                 (remove-let* (L1:Expr:let* xr er b)))]))

(define-compiler compiler
  remove-when
  remove-let*)

(check-equal?
 (compiler (L0:Expr:if #f #t #f))
 (L2:Expr:if #f #t #f))

(check-equal?
 (compiler (L0:Expr:when #f #t))
 (L2:Expr:if #f #t #f))

(check-equal?
 (compiler (L0:Expr:b #t))
 (L2:Expr:b #t))

(check-equal?
 (compiler (L0:Expr:let* `(x y) `(#t #f) 'x))
 (L2:Expr:let '(x) '(#t)
              (L2:Expr:let '(y) '(#f)
                            (L2:Expr:let '() '()
                                         'x))))

(check-equal?
 (compiler (L0:Expr:let '(x y) '(#t #t) #f))
 (L2:Expr:let '(x y) '(#t #t) #f))