#lang racket/base

(require (for-syntax syntax/parse
                     racket/base))

(define-syntax (compiler stx)
  (syntax-parse stx
    [(_ passes ...)
     (quasisyntax/loc stx
       (compose #,@(reverse (attribute passes))))]))
