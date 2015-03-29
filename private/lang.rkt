#lang racket/base

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/match
                     racket/pretty
                     racket/set
                     racket/base
                     unstable/syntax
                     "structs.rkt"
                     "lang-helpers.rkt"))
(provide define-language)

(begin-for-syntax
  (define-syntax-class term
    (pattern (pred:id (name:id ...))))
  (define-syntax-class non-term
    (pattern (name:id
              (~optional (~seq #:alts (alts:id ...)) #:defaults ([(alts 1) null]))
              (~optional ((~seq (~datum +) +prod:production-clause ...))
                         #:defaults ([(+prod 1) null]))
              (~optional ((~seq (~datum -) -prod:production-clause ...))
                         #:defaults ([(-prod 1) null]))
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
       (extend-language* (attribute name)
                         (attribute extend-lang)
                         (attribute entry)
                         (attribute terminals)
                         (attribute +terms)
                         (attribute -terms)
                         (attribute non-terminals)))
     (with-syntax ([(structs ...) (build-lang-structs language stx)])
       (syntax/loc stx
         (begin
           structs ...
           (define-syntax name (extend-language* (quote-syntax name)
                                                 (quote-syntax extend-lang)
                                                 entry
                                                 (syntax->list (quote-syntax (terminals ...)))
                                                 (syntax->list (quote-syntax (+terms ...)))
                                                 (syntax->list (quote-syntax (-terms ...)))
                                                 (syntax->list
                                                  (quote-syntax (non-terminals ...))))))))]))

(define-for-syntax (term->terminal stx)
  (syntax-parse stx
    [term:term
     (terminal (attribute term.pred)
               (for/list ([i (in-list (syntax->list #'(term.name ...)))]) i))]))

(define-for-syntax (non-term->non-terminal stx)
  (syntax-parse stx
    (non-term:non-term
     (non-terminal (attribute non-term.name)
                   #f
                   (for/list ([i (in-list (attribute non-term.alts))]) i)
                   (for/list ([i (in-list (attribute non-term.productions))])
                     (production #f #f null i))
                   #f))))

(define-for-syntax (build-delta stx)
  (syntax-parse stx
    (delta:non-term
     (non-terminal/delta (attribute delta.name)
                         (for/list ([i (in-list (attribute delta.alts))]) i)
                         (for/list ([i (in-list (attribute delta.+prod))]) i)
                         (for/list ([i (in-list (attribute delta.-prod))]) i)))))

(define-for-syntax (prod->production stx production-identifiers l-name nt-name)
  (syntax-parse stx
    [x:id
     (cond [(set-member? production-identifiers
                         (lang-symb-type (symb-split (syntax-e #'x))))
            (production #'x
                        (format-id l-name "~a:~a:~a" l-name nt-name #'x)
                        (list (production-field #'x (symb-split (syntax-e #'x)) 0))
                        stx)])]
    [(name:id body ...)
     (define mem (set-member? production-identifiers
                              (lang-symb-type (symb-split (syntax-e #'namespace)))))
     (define name* (if mem #'#%app #'name))
     (define body* (if mem stx #'(body ...)))
     (production name*
                 (format-id l-name "~a:~a:~a" l-name nt-name name*)
                 ;(format-unique-id stx "~a:~a:~a" l-name nt-name name*)
                 (collect-production-fields body* production-identifiers 0)
                 stx)]))

(define-for-syntax (collect-production-fields body production-identifiers depth)
  (syntax-parse body
    [((subform ...) (~datum ...) rest ...)
     `(,@(collect-production-fields #'(subform ...) production-identifiers (add1 depth))
       ,@(collect-production-fields #'(rest ...) production-identifiers depth))]
    [((subform ...) rest ...)
     `(,@(collect-production-fields #'(subform ...) production-identifiers depth)
       ,@(collect-production-fields #'(rest ...) production-identifiers depth))]
    [(id:id (~datum ...) rest ...)
     (define split (symb-split (syntax-e #'id)))
     (cond [(set-member? production-identifiers
                         (lang-symb-type split))
            `(,(production-field #'id split (add1 depth))
              ,@(collect-production-fields #'(rest ...) production-identifiers depth))]
           [else (collect-production-fields #'(rest ...) production-identifiers depth)])]
    [(id:id rest ...)
     (define split (symb-split (syntax-e #'id)))
     (cond [(set-member? production-identifiers
                         (lang-symb-type split))
            `(,(production-field #'id split depth)
              ,@(collect-production-fields #'(rest ...) production-identifiers depth))]
           [else (collect-production-fields #'(rest ...) production-identifiers depth)])]
    [() '()]))

(define-for-syntax (extend-language* name orig entry terminals +terms -terms non-terminals)
  (define sname (format-id name "~a-struct" name))
  (define language
    (cond
      [(identifier? orig)
       (define base (syntax-local-value orig))
       (define entry* (or entry (lang-entry base)))
       (extend-language base
                        name
                        sname
                        entry*
                        (for/list ([i (in-list +terms)]) (term->terminal i))
                        (for/list ([i (in-list -terms)]) (term->terminal i))
                        (for/list ([i (in-list non-terminals)]) (build-delta i)))]
      [else
       (define non-term* (for/list ([i (in-list non-terminals)]) (non-term->non-terminal i)))
       (define entry* (or entry
                          (non-terminal-name (car non-term*))))
       (lang name
             sname
             entry*
             (for/list ([i (in-list terminals)]) (term->terminal i))
             non-term*)]))
  (define language* (fill-productions language))
  (fill-parser language*))

(define-for-syntax (fill-productions language)
  (define production-identifiers (collect-production-identifiers language))
  (match language
    [(lang name sname entry terminals non-terminals*)
     (lang name sname entry terminals
           (for/list ([i (in-list non-terminals*)])
             (match i
               [(non-terminal name* sname* alts productions* parser)
                (define sname** (format-id name "~a:~a" name name*))
                (non-terminal name* sname** alts
                              (for/list ([i (in-list productions*)])
                                (match i
                                  [(production name*** sname fields pattern)
                                   (prod->production pattern
                                                     production-identifiers
                                                     name
                                                     name*)]))
                              parser)])))]))

(define-for-syntax (fill-parser language)
  (match language
    [(lang name sname entry terminals non-terminals*)
     (lang name sname entry terminals
           (for/list ([i (in-list non-terminals*)])
             (match i
               [(non-terminal name* sname* alts productions* parser*)
                (non-terminal name* sname* alts productions* (build-non-terminal-parser i))])))]))

(define-for-syntax (build-non-terminal-parser non-term)
  (match non-term
    [(non-terminal name sname alts productions parser)
     (define parser #`(lambda (stx) (syntax-parse stx
                                      #,@(for/list ([p (in-list productions)])
                                           (build-production-parser p)))))
     (eval-syntax parser)]))

(define-for-syntax (build-production-parser prod)
  (match prod
    [(production name sname fields pattern)
     #`[#,pattern #'(#,sname #,@fields)]]))

(define-for-syntax (add-unquote pattern fields)
  (syntax-parse pattern
    [((subform ...) rest ...)
     #`(#,(add-unquote #'(subform ...) fields)
         #,@(add-unquote #'(rest ...) fields))]
    [(x:id rest ...)
     (cond [(member #'x fields free-identifier=?)
            #`(,x #,@(add-unquote #'(rest ...) fields))]
           [else #`(x #,@(add-unquote #'(rest ...) fields))])]
    [() #'()]))
