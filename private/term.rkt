#lang racket/base

(provide term)
(require racket/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/match
                     racket/pretty
                     "structs.rkt"
                     "term-helpers.rkt"))

(define-match-expander term
  (lambda (stx)
    (syntax-parse stx
      [(_ (lang:id non-term:id) pattern)
       #'(void)]))
  (lambda (stx)
    (syntax-parse stx
      [(_ (lang:id non-term:id) pattern)
       (define language (syntax-local-value (attribute lang)))
       (define nt (find-non-terminal language (attribute non-term)))
       (syntax-parse (attribute pattern)
         [(unquote #{x : type:id})
          (match (find-non-terminal language (attribute type))
            [(struct* non-terminal ([name name]))
             #'(void)])]
         [(production:id pattern ...)
           #'(void)])])))
