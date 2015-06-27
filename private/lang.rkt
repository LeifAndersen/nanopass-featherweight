#lang racket/base

;; Used for defining languages.
;; Syntax for define-language:
;; (define-language L-name
;;    #:extends [extend-name #f]
;;    #:entry   [entry #f]
;;    #:terminals (<terminal> ...)
;;    <non-terminal ...>)
;;
;; <terminal> ::= (predicate? (<alt> ...)
;;
;; <non-terminal> ::= (<name> #:alts (<alt> ...) <production-rule>)
;;
;; Where predicate is any predicate like boolean? or void?
;;
;; alt is a symbol to refer to the terminal/non-terminal
;;
;; And a production rule is an s-expression containing terminals and non-terminals.

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/match
                     racket/pretty
                     racket/set
                     racket/base
                     unstable/syntax
                     "structs.rkt"
                     "lang-helpers.rkt"))
(provide define-language
         define-extended-language)

;; Syntax-classes used for define-language
(begin-for-syntax
  (define-syntax-class term
    (pattern (pred:id (name:id ...))))
  (define-syntax-class extended-non-term
    (pattern (name:id (alts:id ...)
                      (~or (~seq #:+ +prod:production-clause)
                           (~seq #:- -prod:production-clause))
                      ...)))
  (define-syntax-class non-term
    (pattern (name:id (alts:id ...)
                      productions:production-clause ...)))
  (define-syntax-class production-clause
    (pattern val)
    (pattern (name:id fields ...))))

;; Define-language macro
(define-syntax (define-language stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~optional (~seq #:extends extend-lang:id) #:defaults ([extend-lang #'#f]))
             (~optional (~seq #:entry entry:id) #:defaults ([entry #'#f]))
             (~optional (~seq #:terminals (terminals:term ...))
                        #:defaults ([(terminals 1) null])))
        ...
        non-terminals:non-term ...)
     (define language
       (build-language* (attribute name)
                        (attribute entry)
                        (attribute terminals)
                        (attribute non-terminals)))
     (with-syntax ([(structs ...) (build-lang-structs language stx)])
       (syntax/loc stx
         (begin
           structs ...
           (define-syntax name (build-language* (quote-syntax name)
                                                (quote-syntax entry)
                                                (syntax->list (quote-syntax (terminals ...)))
                                                (syntax->list
                                                 (quote-syntax (non-terminals ...))))))))]))

(define-syntax (define-extended-language stx)
  (syntax-parse stx
    [(_ name:id extend-lang:id
        (~or (~optional (~seq #:entry entry:id) #:defaults ([entry #'#f]))
             (~optional (~seq #:terminals ((~or (~seq #:+ +terms:term)
                                                (~seq #:- -terms:term))
                                           ...))))
        ...
        non-terminals:extended-non-term ...)
     (define language
       (extend-language* (attribute name)
                         (attribute extend-lang)
                         (attribute entry)
                         (attribute +terms)
                         (attribute -terms)
                         (attribute non-terminals)))
     (with-syntax ([(structs ...) (build-lang-structs language stx)])
       (syntax/loc stx
         (begin
           structs ...
           (define-syntax name (extend-language* (quote-syntax name)
                                                 (quote-syntax extend-lang)
                                                 (quote-syntax entry)
                                                 (syntax->list (quote-syntax (+terms ...)))
                                                 (syntax->list (quote-syntax (-terms ...)))
                                                 (syntax->list
                                                  (quote-syntax (non-terminals ...))))))))]))

;; Convert a term syntax class to a terminal struct.
(define-for-syntax (term->terminal stx)
  (syntax-parse stx
    [term:term
     (terminal (attribute term.pred)
               (for/list ([i (in-list (syntax->list #'(term.name ...)))]) i))]))

;; Convert a non-terminal syntax class to a non-terminal struct.
(define-for-syntax (non-term->non-terminal stx)
  (syntax-parse stx
    (non-term:non-term
     (non-terminal (attribute non-term.name)
                   #f
                   (for/list ([i (in-list (attribute non-term.alts))]) i)
                   (for/list ([i (in-list (attribute non-term.productions))])
                     (production #f #f null i))
                   #f))))

;; Create delta struct for a non-terminal based on +/- syntax.
(define-for-syntax (build-delta stx)
  (syntax-parse stx
    (delta:extended-non-term
     (non-terminal/delta (attribute delta.name)
                         (for/list ([i (in-list (attribute delta.alts))]) i)
                         (for/list ([i (in-list (attribute delta.+prod))]) i)
                         (for/list ([i (in-list (attribute delta.-prod))]) i)))))

;; Create a production rule from a production syntax class.
;; Also gather information on the patterns for use later.
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
                              (lang-symb-type (symb-split (syntax-e #'name)))))
     (define name* (if mem #'#%app #'name))
     (define body* (if mem stx #'(body ...)))
     (production name*
                 (format-id l-name "~a:~a:~a" l-name nt-name name*)
                 ;(format-unique-id stx "~a:~a:~a" l-name nt-name name*)
                 (collect-production-fields body* production-identifiers 0)
                 stx)]))

;; Collect all of the fields in a production rule.
;; For use by prod->production
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

(define-for-syntax (build-language* name entry terminals non-terminals)
  (define sname (format-id name "~a-struct" name))
  (define non-term* (for/list ([i (in-list non-terminals)]) (non-term->non-terminal i)))
  (define entry* (or entry
                       (non-terminal-name (car non-term*))))
  (define language
    (lang name
          sname
          entry*
          (for/list ([i (in-list terminals)]) (term->terminal i))
          non-term*))
  (define language* (fill-productions language))
  (fill-parser language*))

;; Create an extended language.
(define-for-syntax (extend-language* name orig entry +terms -terms non-terminals)
  (define sname (format-id name "~a-struct" name))
  (define base (syntax-local-value orig))
  (define entry* (or entry (lang-entry base)))
  (define language
    (extend-language base
                     name
                     sname
                     entry*
                     (for/list ([i (in-list +terms)]) (term->terminal i))
                     (for/list ([i (in-list -terms)]) (term->terminal i))
                     (for/list ([i (in-list non-terminals)]) (build-delta i))))
  (define language* (fill-productions language))
  (fill-parser language*))

;; Fill productions with information about production rules.
;; Used because production structs created in two passes.
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

;; Used for creating parser for pattern matching.
;; Currently does not match (...) patterns.
(define-for-syntax (fill-parser language)
  (match language
    [(lang name sname entry terminals non-terminals*)
     (lang name sname entry terminals
           (for/list ([i (in-list non-terminals*)])
             (match i
               [(non-terminal name* sname* alts productions* parser*)
                (non-terminal name* sname* alts productions* (build-non-terminal-parser i))])))]))

;; Build parser for non-terminals
;; Currently does awful eval because it builds up match.
(define-for-syntax (build-non-terminal-parser non-term)
  (match non-term
    [(non-terminal name sname alts productions parser)
     (define parser #'(lambda (stx) stx))
       #;#`(lambda (stx) (syntax-parse stx
                           #,@(for/list ([p (in-list productions)])
                                (build-production-parser p))))
     (eval-syntax parser)]))

;; Build piece of parser for production rule.
;; Returns syntax object to be put together by build-non-terminal-parser.
(define-for-syntax (build-production-parser prod)
  (match prod
    [(production name sname fields pattern)
     #`[#,pattern #'(#,sname #,@fields)]]))

;; Adds unquotes to syntax provided by build-production-parser.
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
