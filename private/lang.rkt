#lang racket

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/match
                     racket/pretty
                     "structs.rkt"
                     "lang-helpers.rkt"))
(provide define-language)

(begin-for-syntax
  (define-syntax-class term
    (pattern (pred:id (name:id ...))))
  (define-syntax-class non-term
    (pattern (name:id
              (~optional (~seq #:alts (alts:id ...)) #:defaults ([(alts 1) null]))
              (~optional ((~seq (~datum +) +pord:production-clause ...)))
              (~optional ((~seq (~datum -) -prod:production-clause ...)))
              productions:production-clause ...)))
  (define-syntax-class production-clause
    (pattern val)))

(define-syntax (define-language stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~once (~optional (~seq #:extends extend-lang:id)))
             (~once (~optional (~seq #:entry entry:id)))
             (~once (~optional (~seq #:terminals
                                     (~or (terminals:term ...)
                                          (~seq (~optional ((~datum +) +terms:term ...)
                                                           #:defaults ([(+terms 1) null]))
                                                (~optional (~seq (~datum -) -terms:term ...)
                                                           #:defaults ([(-terms 1) null])))))
                               #:defaults ([(terminals 1) null]))))
        ...
        non-terminals:non-term ...)
     (define language
       (cond
         [(identifier? (attribute extend-lang))
          (define base (syntax-local-value #'extend-lang))
          (extend-language base
                           (attribute name)
                           (attribute entry)
                           (for/list ([i (in-list (attribute +terms))]) (term->terminal i))
                           (for/list ([i (in-list (attribute -terms))]) (term->terminal i))
                           null)]
         [else
          (lang (attribute name)
                (attribute entry)
                (for/list ([i (in-list (attribute terminals))]) (term->terminal i))
                (for/list ([i (in-list (attribute non-terminals))]) (non-term->non-terminal i)))]))
     ;(pretty-write language)
     (with-syntax ([(structs ...) (build-lang-structs language stx)])
       #`(begin
           structs ...
           (define-syntax name #,language)))]))

(define-for-syntax (term->terminal stx)
  (syntax-parse stx
    [term:term
     (terminal (attribute term.pred)
               (for/list ([i (in-list (syntax->list #'(term.name ...)))]) i))]))

(define-for-syntax (non-term->non-terminal stx)
  (syntax-parse stx
    (non-term:non-term
     (non-terminal (attribute non-term.name)
                   (for/list ([i (in-list (attribute non-term.alts))]) i)
                   (for/list ([i (in-list (attribute non-term.productions))]) i)))))

(define-for-syntax (build-lang-structs language stx)
  (match language
    [(lang name entry terminals non-terminals)
     (define name* (format-id stx "~a-struct" name))
     #`((struct #,name* ())
        #,@(for/list ([non-t (in-list non-terminals)])
             (match non-t
               [(non-terminal non-t-name alts productions)
                (define non-t-name* (format-id stx "~a:~a" name non-t-name))
                #`(struct #,non-t-name* #,name* ())])))]))

#;
(define-language L0
  (terminals
   (number (n)))
  (Expr ()
    (+ n1 n2)))

;(define-language L*)
#;
(module+ test
  ;(define-language L*)
  (define-language L0
    #:terminals ((number? (x)))
    )
  #;(define-language L1
    #:extends L0
    #:terminals (+ (boolean? (b)))
    )
  )
