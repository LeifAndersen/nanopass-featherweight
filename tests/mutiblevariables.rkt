#lang racket

(require "../main.rkt"
         rackunit)

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

(define-language L1
  #:extends L0
  (Expr
   (+ (box x)
      (unbox x)
      (set-box! x e))
   (- (set! x e))))

(define-pass collect-mv : L0 -> L0
  (Expr : Expr -> Expr
    [(L0:Expr:set! x e)
     (set-assigned-variable! x)
     (L0:Expr:set! x (collect-mv:Expr e))]))

(define-pass eliminate-mv : L0 -> L1
  (Expr : Expr -> Expr
    [(L0:Expr:x x)
     #:when (variable-assigned? x)
     (L1:Expr:unbox (eliminate-mv:Expr x))]
    [(L0:Expr:set! x e)
     (L1:Expr:set-box! (L1:Expr:x (L0:Expr:x-x x))
                       (eliminate-mv:Expr e))]
    [(L0:Expr:lambda x e)
     (cond
       [(variable-assigned? x)
        (define x* (L0:Expr:x-x x) #;(gensym))
        (L1:Expr:lambda (L1:Expr:x x*)
                        (L1:Expr:let (L1:Expr:x (L0:Expr:x-x x))
                                     (L1:Expr:box (L1:Expr:x x*))
                                     (eliminate-mv:Expr e)))]
       [else (L1:Expr:lambda (eliminate-mv:Expr x)
                             (eliminate-mv:Expr e))])]))

(define-compiler mve
  collect-mv
  eliminate-mv)

(reset-assigned-variables!)
(check-equal?
 (mve (L0:Expr:lambda (L0:Expr:x 'x)
                      (L0:Expr:set! (L0:Expr:x 'x)
                                    (L0:Expr:n 5))))
 (L1:Expr:lambda (L1:Expr:x 'x)
                 (L1:Expr:let (L1:Expr:x 'x)
                              (L1:Expr:box (L1:Expr:x 'x))
                              (L1:Expr:set-box! (L1:Expr:x 'x)
                                                (L1:Expr:n 5)))))

(reset-assigned-variables!)
(check-equal?
 (mve (L0:Expr:lambda (L0:Expr:x 'x) (L0:Expr:n 5)))
 (L1:Expr:lambda (L1:Expr:x 'x) (L1:Expr:n 5)))


(define (set-assigned-variable! x)
  (set-add! mv-table x))

(define (reset-assigned-variables!)
  (set! mv-table (mutable-set)))

(define (variable-assigned? x)
  (set-member? mv-table x))

(define mv-table (mutable-set))