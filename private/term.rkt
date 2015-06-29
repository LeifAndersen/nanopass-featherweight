#lang racket/base

(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(define-match-expander term
  (lambda (stx)
    (syntax-parse stx
      [(_ (lang:id non-terminal:id) pattern)
       (define lang (syntax-local-value #'lang))
       (syntax-parse pattern
         ...)]
  (lambda (stx)
    (syntax-parse stx
      [(_ (lang:id non-terminal:id) pattern)
       #'(void)])))
