#lang racket/base

(require (for-syntax syntax/parse
                     racket/base))

(provide define-compiler)

(define-syntax (define-compiler stx)
  (syntax-parse stx
    [(_ name:id passes:id ...)
     (quasisyntax/loc stx
       (define name
         (compose #,@(reverse (attribute passes)))))]))
