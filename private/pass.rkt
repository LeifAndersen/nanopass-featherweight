#lang racket

(require (for-syntax syntax/parse
                     racket/syntax
                     "structs.rkt"
                     "pass-helpers.rkt"))
(provide define-pass)

(begin-for-syntax
  (define-syntax-class transform
    (pattern (name:id (~datum :) (~or NTin-name:id #f) (~datum ->)
                      (~or NTout-name:id #f) rules:rule ...)))
  (define-syntax-class rule
    (pattern x:id)))

(define-syntax (define-pass stx)
  (syntax-parse stx
    [(_ name:id (~datum :) (~or Li-name:id #f) (~datum ->) (~or Lo-name:id #f) trans:transform ...)
     (define Li (lookup-lang (attribute Li-name)))
     (define Lo (lookup-lang (attribute Lo-name)))
     (with-syntax ([(transformers ...) (build-transformer stx)])
       #`(begin
           transformers ...))]))

(define-for-syntax (lookup-lang lang)
  (cond [(identifier? lang)
         (syntax-local-value lang)]
        [else #f]))

(define-for-syntax (build-transformer stx)
  #'(void))
