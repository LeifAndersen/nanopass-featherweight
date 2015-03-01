#lang racket

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/match
                     racket/pretty
                     racket/set
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
        (~or (~once (~optional (~seq #:extends extend-lang:id) #:defaults ([extend-lang #'#f])))
             (~once (~optional (~seq #:entry entry:id) #:defaults ([entry #'#f])))
             (~once (~optional (~seq #:terminals (terminals:term ...))
                               #:defaults ([(terminals 1) null])))
             (~once (~optional (~seq #:+terminals (+terms:term ...))
                               #:defaults ([(+terms 1) null])))
             (~once (~optional (~seq #:-terminals (-terms:term ...))
                               #:defaults ([(-terms 1) null]))))
        ...
        non-terminals:non-term ...)
     (define language
       (extend-language* (attribute name) (attribute extend-lang) (attribute entry)
                         (attribute terminals) (attribute +terms) (attribute -terms) (attribute non-terminals)))
     (with-syntax ([(structs ...) (build-lang-structs language stx)])
       #`(begin
           structs ...
           (define-syntax name (extend-language* #'name #'extend-lang entry
                                                 (syntax->list #'(terminals ...))
                                                 (syntax->list #'(+terms ...))
                                                 (syntax->list #'(-terms ...))
                                                 (syntax->list #'(non-terminals ...))))))]))

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

(define-for-syntax (extend-language* name orig entry terminals +terms -terms non-terminals)
  (cond
    [(identifier? orig)
     (define base (syntax-local-value orig))
     (extend-language base
                      name
                      entry
                      (for/list ([i (in-list +terms)]) (term->terminal i))
                      (for/list ([i (in-list -terms)]) (term->terminal i))
                      null)]
    [else
     (lang name
           entry
           (for/list ([i (in-list terminals)]) (term->terminal i))
           (for/list ([i (in-list non-terminals)]) (non-term->non-terminal i)))]))

(define-for-syntax (build-lang-structs language stx)
  (define production-identifiers (collect-production-identifiers language))
  (match language
    [(lang name entry terminals non-terminals)
     (define name* (format-id stx "~a-struct" name))
     #`((struct #,name* ())
        #,@(for/list ([non-t (in-list non-terminals)])
             (match non-t
               [(non-terminal non-t-name alts productions)
                (define non-t-name* (format-id stx "~a:~a" name non-t-name))
                #`(begin
                    (struct #,non-t-name* #,name* ())
                    #,@(for/list ([rule (in-list productions)])
                         (syntax-parse rule
                           [x:id
                            (cond [(set-member? production-identifiers
                                                (lang-symb-type (symb-split (syntax-e #'x))))
                                   (define p-name (format-id stx "~a:~a" non-t-name* rule))
                                   #`(struct #,p-name #,non-t-name* (#,rule))])]
                           [(name:id body ...)
                            (cond [(not (set-member? production-identifiers
                                                     (lang-symb-type (symb-split (syntax-e #'namespace)))))
                                   (define p-name (format-id stx "~a:~a" non-t-name* #'name))
                                   #`(struct #,p-name #,non-t-name*
                                       (#,@(collect-terminals #'(body ...) production-identifiers)))])])))])))]))

(define-for-syntax (collect-terminals body production-identifiers)
  ;(displayln body)
  (syntax-parse body
    [((subform ...) rest ...)
     `(,@(collect-terminals (attribute subform) production-identifiers)
       ,@(collect-terminals (attribute rest) production-identifiers))]
    [(id:id rest ...)
     (cond [(set-member? production-identifiers
                         (lang-symb-type (symb-split (syntax-e #'id))))
            `(,#'id ,@(collect-terminals (attribute rest) production-identifiers))]
           [else (collect-terminals (attribute rest) production-identifiers)])]
    [() '()]))

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
