#lang racket

(require (for-syntax syntax/parse
                     "structs.rkt"
                     "lang-helpers.rkt"))
(provide define-language)

(begin-for-syntax
  (define-syntax-class term
    (pattern (pred:id (name:id ...))))
  (define-syntax-class non-term
    (pattern (name:id
              (~optional (~seq #:alts (alts:id ...)))
              (~or (~seq productions:production-clause ...)
                   (~optional (~seq (~datum +) +pord:production-clause ...))
                   (~optional (~seq (~datum -) -prod:production-clause ...))))))
  (define-syntax-class production-clause
    (pattern val)))

(define-syntax (define-language stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~once (~optional (~seq #:extends extend-lang:id)))
             (~once (~optional (~seq #:entry entry:id)))
             (~once (~optional (~seq #:terminals
                                     (~or (terminals:term ...)
                                          (~seq (~optional (~seq (~datum +) +terms:term ...))
                                                (~optional (~seq (~datum -) -terms:term ...)))))
                               #:defaults ([(terminals 1) #'null]))))
        ...
        non-terminals:non-term ...)
     (define language
       (cond
         [(identifier? (attribute extend-lang))
          (define base (syntax-local-value #'extend-lang))
          (extend-language base #'name #'entry null null null)]
         [else
          (lang (attribute name)
                (attribute entry)
                (for/list ([i (in-list (syntax->list #'(terminals ...)))]) (term->terminal i))
                null)]))
     #'#f]))

(define-for-syntax (term->terminal stx)
  (syntax-parse stx
    [term:term
     (terminal #'term.pred
               (for/list ([i (in-list (syntax->list #'(term.name ...)))]) i))]))
