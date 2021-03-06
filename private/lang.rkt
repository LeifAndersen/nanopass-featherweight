#lang racket/base

;; Used for defining languages.
;; Syntax for define-language:
;; (define-language L-name
;;    #:extends [extend-name #f]
;;    #:entry   [entry #f]
;;    <non-terminal ...>)
;;
;; <non-terminal> ::= (<name> <production-rule> ...)
;;
;; Where predicate is any predicate like boolean? or void?
;;
;; alt is a symbol to refer to the terminal/non-terminal
;;
;; And a production rule is an s-expression containing terminals and non-terminals.

(provide define-language
         define-extended-language)

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/match
                     racket/pretty
                     racket/set
                     racket/base
                     unstable/syntax
                     "structs.rkt"
                     "lang-helpers.rkt"))

;; Syntax-classes used for define-language
(begin-for-syntax
  (define-syntax-class extended-non-term
    (pattern (name:id (~or (~seq #:+ +prod:production-clause)
                           (~seq #:- -prod:production-clause))
                      ...)))
  (define-syntax-class non-term
    (pattern (name:id productions:production-clause ...)))
  (define-syntax-class production-clause
    (pattern val:id)
    (pattern (name:id fields ...))))

;; Define-language macro
(define-syntax (define-language stx)
  (syntax-parse stx
    [(_ name:id
        (~optional (~seq #:entry entry:id) #:defaults ([entry #'#f]))
        non-terminals:non-term ...)
     (define language
       (build-language* (attribute name)
                        (attribute entry)
                        (attribute non-terminals)))
     (with-syntax ([(structs ...) (build-lang-structs language stx)])
       (syntax/loc stx
         (begin
           structs ...
           (define-syntax name (build-language* (quote-syntax name)
                                                (quote-syntax entry)
                                                (syntax->list
                                                 (quote-syntax (non-terminals ...))))))))]))

(define-syntax (define-extended-language stx)
  (syntax-parse stx
    [(_ name:id extend-lang:id
        (~optional (~seq #:entry entry:id) #:defaults ([entry #'#f]))
        non-terminals:extended-non-term ...)
     (define language
       (extend-language* (attribute name)
                         (attribute extend-lang)
                         (attribute entry)
                         (attribute non-terminals)))
     (with-syntax ([(structs ...) (build-lang-structs language stx)])
       (syntax/loc stx
         (begin
           structs ...
           (define-syntax name (extend-language* (quote-syntax name)
                                                 (quote-syntax extend-lang)
                                                 (quote-syntax entry)
                                                 (syntax->list
                                                  (quote-syntax (non-terminals ...))))))))]))

;; Convert a non-terminal syntax class to a non-terminal struct.
(define-for-syntax (non-term->non-terminal stx)
  (syntax-parse stx
    (non-term:non-term
     (non-terminal (attribute non-term.name)
                   #f
                   (for/list ([i (in-list (attribute non-term.productions))])
                     (production #f #f null i))))))

;; Create delta struct for a non-terminal based on +/- syntax.
(define-for-syntax (build-delta stx)
  (syntax-parse stx
    (delta:extended-non-term
     (non-terminal/delta (attribute delta.name)
                         (for/list ([i (in-list (attribute delta.+prod))]) i)
                         (for/list ([i (in-list (attribute delta.-prod))]) i)))))

;; Create a production rule from a production syntax class.
;; Also gather information on the patterns for use later.
(define-for-syntax (prod->production stx production-identifiers l-name nt-name)
  (syntax-parse stx
    [x:id
     #:when (or (set-member? production-identifiers #'x) (identifier-binding #'x))
     (production #'x
                 (format-id l-name "~a:~a:term:~a" l-name nt-name #'x)
                 (list (production-field #'x (format-id stx "~a_~a" #'x (gensym)) 0))
                 stx)]
    [(name:id body ...)
     (production #'name
                 (format-id l-name "~a:~a:~a" l-name nt-name #'name)
                 ;(format-unique-id stx "~a:~a:~a" l-name nt-name name*)
                 (collect-production-fields #'(body ...) production-identifiers 0)
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
     #:when (or (set-member? production-identifiers #'id) (identifier-binding #'id))
     `(,(production-field #'id (format-id body "~a_~a" #'id (gensym)) (add1 depth))
       ,@(collect-production-fields #'(rest ...) production-identifiers depth))]
    [(id:id rest ...)
     #:when (or (set-member? production-identifiers #'id) (identifier-binding #'id))
     `(,(production-field #'id (format-id body "~a_~a" #'id (gensym)) depth)
       ,@(collect-production-fields #'(rest ...) production-identifiers depth))]
    [() '()]))

(define-for-syntax (build-language* name entry non-terminals)
  (define sname (format-id name "~a-struct" name))
  (define non-term* (for/list ([i (in-list non-terminals)]) (non-term->non-terminal i)))
  (define entry* (or entry
                       (non-terminal-name (car non-term*))))
  (define language
    (lang name
          sname
          entry*
          non-term*))
  (fill-productions language))

;; Create an extended language.
(define-for-syntax (extend-language* name orig entry non-terminals)
  (define sname (format-id name "~a-struct" name))
  (define base (syntax-local-value orig))
  (define entry* (or entry (lang-entry base)))
  (define language
    (extend-language base
                     name
                     sname
                     entry*
                     (for/list ([i (in-list non-terminals)]) (build-delta i))))
  (fill-productions language))

;; Fill productions with information about production rules.
;; Used because production structs created in two passes.
(define-for-syntax (fill-productions language)
  (define production-identifiers (collect-production-identifiers language))
  (match language
    [(lang name sname entry non-terminals*)
     (lang name sname entry
           (for/list ([i (in-list non-terminals*)])
             (match i
               [(non-terminal name* sname* productions*)
                (define sname** (format-id name "~a:~a" name name*))
                (non-terminal name* sname**
                              (for/list ([i (in-list productions*)])
                                (match i
                                  [(production name*** sname fields pattern)
                                   (prod->production pattern
                                                     production-identifiers
                                                     name
                                                     name*)])))])))]))
