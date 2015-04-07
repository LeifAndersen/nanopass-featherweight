#lang racket

; Compiler pass that does mutable variable elimination

(require "../main.rkt"
         rackunit)

; L0, set!, lambda, let, application
(define-language L0
  #:terminals ((symbol? (x))
               (number? (n)))
  (Expr
   #:alts (e)
   x
   n
   (set! x e)
   (lambda (x) e)
   (let ((x e_1)) e_2)
   (e_1 e_2)))

; Remove variables
; set-box!, box, unbox, lambda, let, application
(define-language L1
  #:extends L0
  (Expr
   (+ (box x)
      (unbox x)
      (set-box! x e))
   (- (set! x e))))

; Collect mutable variables
(define-pass collect-mv : L0 -> L0
  (Expr : Expr -> Expr
    [(L0:Expr:set! x e)
     (set-assigned-variable! x)
     (L0:Expr:set! x (collect-mv:Expr e))]))

; Use collected information to eliminate mutable variables
(define-pass eliminate-mv : L0 -> L1
  (Expr : Expr -> Expr
    [(L0:Expr:x x)
     #:when (variable-assigned? x)
     (L1:Expr:unbox (eliminate-mv:Expr x))]
    [(L0:Expr:set! x e)
     (L1:Expr:set-box! x
                       (eliminate-mv:Expr e))]
    [(L0:Expr:let x e body)
     (cond
       [(variable-assigned? x)
        (define x* x #;(gensym))
        (L1:Expr:let x*
                    (L1:Expr:let x
                                 (L1:Expr:box (L1:Expr:x x*))
                                 (eliminate-mv:Expr e)))]
       [else
        (L1:Expr:let x
                     (eliminate-mv:Expr e)
                     (eliminate-mv:Expr body))])]
    [(L0:Expr:lambda x e)
     (cond
       [(variable-assigned? x)
        (define x* x #;(gensym))
        (L1:Expr:lambda x*
                        (L1:Expr:let x
                                     (L1:Expr:box (L1:Expr:x x*))
                                     (eliminate-mv:Expr e)))]
       [else (L1:Expr:lambda x (eliminate-mv:Expr e))])]))

(define-compiler mve
  collect-mv
  eliminate-mv)

; ---------------------------------------------------
; Implementation of assigned variables table
; Not how commercial nanopass stores variables.

; Add assigned variables.
(define (set-assigned-variable! x)
  (set-add! mv-table x))

; Reset the assigned variable table
; Really shouldn't need this if this was a
; proper implementation.
(define (reset-assigned-variables!)
  (set! mv-table (mutable-set)))

; Determine if a variable is assigned.
(define (variable-assigned? x)
  (set-member? mv-table x))

; The table.
(define mv-table (mutable-set))
; ---------------------------------------------------

; Unit tests
(reset-assigned-variables!)
(check-equal?
 (mve (L0:Expr:lambda 'x
                      (L0:Expr:set! 'x
                                    (L0:Expr:n 5))))
 (L1:Expr:lambda 'x
                 (L1:Expr:let 'x
                              (L1:Expr:box (L1:Expr:x 'x))
                              (L1:Expr:set-box! 'x
                                                (L1:Expr:n 5)))))

(reset-assigned-variables!)
(check-equal?
 (mve (L0:Expr:lambda 'x (L0:Expr:n 5)))
 (L1:Expr:lambda 'x (L1:Expr:n 5)))

