#lang racket/base

;; Implementation for define-compiler macro.
;;
;; Syntax:
;; (define-compiler <pass> ...)
;;
;; passes the source from one argument into the next,
;; returning the result of the final pass.

(require (for-syntax syntax/parse
                     racket/base))

(provide define-compiler)

;; define-compiler macro, syntax at top of file.
(define-syntax (define-compiler stx)
  (syntax-parse stx
    [(_ name:id passes:id ...)
     (quasisyntax/loc stx
       (define name
         (compose #,@(reverse (attribute passes)))))]))
