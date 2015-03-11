#lang racket

(require (for-syntax syntax/parse
                     racket/syntax
                     "structs.rkt"
                     "pass-helpers.rkt"))
(provide define-pass)

(begin-for-syntax
  (define-syntax-class transform
    (pattern (name:id (~datum :) (~or NTin-name:id #f) (~datum ->) (~or NTout-name:id #f)
                      (~or (~once (~optional (~seq #:formals (formals:id ...))
                                             #:defaults ([(formals 1) null])))
                           (~once (~optional (~seq #:returns (returns:id ...))
                                             #:defaults ([(returns 1) null]))))
                      rules:rule ...)))
  (define-syntax-class rule
    (pattern x:id)))

(define-syntax (define-pass stx)
  (syntax-parse stx
    [(_ name:id (~datum :) (~or Li-name:id #f) (~datum ->) (~or Lo-name:id #f)
        (~or (~once (~optional (~seq #:formals (formals:id ...))
                               #:defaults ([(formals 1) null])))
             (~once (~optional (~seq #:return (returns:id ...))
                               #:defaults ([(returns 1) null])))
             (~once (~optional (~seq #:definitions (definitions ...))
                               #:defaults ([(definitions 1) null]))))
        trans:transform ...)
     (define Li (lookup-lang (attribute Li-name)))
     (define Lo (lookup-lang (attribute Lo-name)))
     (with-syntax ([(transformers ...) (build-transformers stx)])
       #`(begin
           transformers ...))]))

(define-for-syntax (lookup-lang lang)
  (cond [(identifier? lang)
         (syntax-local-value lang)]
        [else #f]))

(define-for-syntax (build-transformers stx)
  #'(void))
