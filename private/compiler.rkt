#lang racket/base

(require (for-syntax syntax/parse
                     racket/base))

(provide define-compiler)

(define-syntax (define-compiler stx)
  (syntax-parse stx
    [(_ passes ...)
     (quasisyntax/loc stx
       (compose #,@(reverse (attribute passes))))]))
