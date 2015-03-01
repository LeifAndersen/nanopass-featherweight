#lang racket

(module+ test
  (require "../main.rkt")

  (define-language L0
    #:terminals ((number? (x))
                 (symbol? (v)))
    (Expr
        #:alts (e)
        x
        (+ x_1 x_2)
        (let ([v e_1] #;...)
          e_2)))

  (define-language L1
    #:extends L0
    #:+terminals ((boolean? (y)))
    (Expr
        #:alts (w)
        (+ y
           (and w_1 e_2)))))
