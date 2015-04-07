#lang racket

; Test composition of passes

(require "../main.rkt"
         rackunit)

; Language L0, let, let*, when, and if.
(define-language L0
  #:terminals ((bool? (b))
               (symbol? (x)))
  (Expr
   #:alts (e)
   b
   x
   (let ([x_1 e_1] ...) e_2)
   (let* ([x_1 e_1] ...) e_2)
   (when e_1 e_2)
   (if e_1 e_2 e_3)))

; Language L1, if, let, let*
(define-language L1
  #:extends L0
  (Expr
   (- (when e_1 e_2))))

; Language L1, remove let*
(define-language L2
  #:extends L1
  (Expr
   (- (let* ([x_1 e_1] ...) e_2))))

; Remove when blocks
; (when e0 e1) -> (if e0 e1 (void))
(define-pass remove-when : L0 -> L1
  (Expr : Expr -> Expr
   [(L0:Expr:when e_1 e_2)
    (L1:Expr:if (remove-when:Expr e_1)
                (remove-when:Expr e_2)
                (L1:Expr:b #f))]))

; turn let* into let
; (let* ([x x* ...]) ([e e* ...]) body)
; ->
; (let ([x]) ([e]) (let* ([x* ...]) ([e* ...]) body))
(define-pass remove-let* : L1 -> L2
  (Expr : Expr -> Expr
   [(L1:Expr:let* '() '() b)
    (L2:Expr:let '() '() (remove-let*:Expr b))]
   [(L1:Expr:let* `(,x ,xr ...) `(,e ,er ...) b)
    (L2:Expr:let `(,x) `(,(remove-let*:Expr e))
                 (remove-let* (L1:Expr:let* xr er b)))]))

; Create a compiler from previous two passes.
(define-compiler compiler
  remove-when
  remove-let*)

; ---------------------------------------------------
; Unit tests
(check-equal?
 (compiler (L0:Expr:if (L0:Expr:b #f)
                       (L0:Expr:b #t)
                       (L0:Expr:b #f)))
 (L2:Expr:if (L2:Expr:b #f)
             (L2:Expr:b #t)
             (L2:Expr:b #f)))

(check-equal?
 (compiler (L0:Expr:when (L0:Expr:b #f)
                         (L0:Expr:b #t)))
 (L2:Expr:if (L2:Expr:b #f)
             (L2:Expr:b #t)
             (L2:Expr:b #f)))

(check-equal?
 (compiler (L0:Expr:b #t))
 (L2:Expr:b #t))

(check-equal?
 (compiler (L0:Expr:let* `(x y) `(,(L0:Expr:b #t)
                                  ,(L0:Expr:b #f))
                         (L0:Expr:x 'x)))
 (L2:Expr:let `(x) `(,(L2:Expr:b #t))
              (L2:Expr:let '(y) `(,(L2:Expr:b #f))
                            (L2:Expr:let '() '()
                                         (L2:Expr:x 'x)))))

(check-equal?
 (compiler (L0:Expr:let '(x y) `(,(L0:Expr:b #t)
                                 ,(L0:Expr:b #t))
                        (L0:Expr:b #f)))
 (L2:Expr:let '(x y) `(,(L2:Expr:b #t)
                       ,(L2:Expr:b #t))
              (L2:Expr:b #f)))
