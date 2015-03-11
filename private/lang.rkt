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
              (~or (~once (~optional (~seq #:alts (alts:id ...)) #:defaults ([(alts 1) null])))
                   (~once (~optional ((~seq (~datum +) +prod:production-clause ...))
                                     #:defaults ([(+prod 1) null])))
                   (~once (~optional ((~seq (~datum -) -prod:production-clause ...))
                                     #:defaults ([(-prod 1) null]))))
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
                         (attribute terminals) (attribute +terms) (attribute -terms)
                         (attribute non-terminals)))
     (with-syntax ([(structs ...) (build-lang-structs language stx)])
       #`(begin
           structs ...
           (define-syntax name (extend-language* (quote-syntax name)
                                                 (quote-syntax extend-lang)
                                                 entry
                                                 (syntax->list (quote-syntax (terminals ...)))
                                                 (syntax->list (quote-syntax (+terms ...)))
                                                 (syntax->list (quote-syntax (-terms ...)))
                                                 (syntax->list (quote-syntax (non-terminals ...)))))))]))

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
                   (for/list ([i (in-list (attribute non-term.productions))])
                     (production #f null i))))))

(define-for-syntax (build-delta stx)
  (syntax-parse stx
    (delta:non-term
     (non-terminal/delta (attribute delta.name)
                         (for/list ([i (in-list (attribute delta.alts))]) i)
                         (for/list ([i (in-list (attribute delta.+prod))]) i)
                         (for/list ([i (in-list (attribute delta.-prod))]) i)))))

(define-for-syntax (prod->production stx production-identifiers)
  (syntax-parse stx
    [x:id
     (cond [(set-member? production-identifiers
                         (lang-symb-type (symb-split (syntax-e #'x))))
            (production #'x null stx)])]
    [(name:id body ...)
     (cond [(not (set-member? production-identifiers
                              (lang-symb-type (symb-split (syntax-e #'namespace)))))
            (production #'name (collect-production-fields (attribute body)
                                                          production-identifiers)
                        stx)])]))

(define-for-syntax (collect-production-fields body production-identifiers)
  (syntax-parse body
    [((subform ...) rest ...)
     `(,@(collect-production-fields (attribute subform) production-identifiers)
       ,@(collect-production-fields (attribute rest) production-identifiers))]
    [(id:id rest ...)
     (cond [(set-member? production-identifiers
                         (lang-symb-type (symb-split (syntax-e #'id))))
            `(,#'id ,@(collect-production-fields (attribute rest) production-identifiers))]
           [else (collect-production-fields (attribute rest) production-identifiers)])]
    [() '()]))

(define-for-syntax (extend-language* name orig entry terminals +terms -terms non-terminals)
  (define language
    (cond
      [(identifier? orig)
       (define base (syntax-local-value orig))
       (extend-language base
                        name
                        entry
                        (for/list ([i (in-list +terms)]) (term->terminal i))
                        (for/list ([i (in-list -terms)]) (term->terminal i))
                        (for/list ([i (in-list non-terminals)]) (build-delta i)))]
      [else
       (lang name
             entry
             (for/list ([i (in-list terminals)]) (term->terminal i))
             (for/list ([i (in-list non-terminals)]) (non-term->non-terminal i)))]))
    (fill-productions language))

(define-for-syntax (fill-productions language)
  (define production-identifiers (collect-production-identifiers language))
  (match language
    [(lang name entry terminals non-terminals*)
     (lang name entry terminals
           (for/list ([i (in-list non-terminals*)])
             (match i
               [(non-terminal name alts productions*)
                (non-terminal name alts
                              (for/list ([i (in-list productions*)])
                                (match i
                                  [(production name fields pattern)
                                   (prod->production pattern production-identifiers)])))])))]))
