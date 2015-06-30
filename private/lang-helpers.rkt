#lang typed/racket/base/no-check

;; Helper functions for lang.rkt
;; In separate module because it's implemented in typed/racket.
;; However, typed/racket cannot produce correct contracts for syntax,
;; so the type checker is not run at compile time.
;; (Thus it's likely that the type annotations are out of date.)

(require syntax/parse
         racket/match
         racket/list
         unstable/syntax
         syntax/id-set
         (only-in racket/set
                  set
                  set-union)
         "structs.rkt"
         "helpers.rkt"
         (for-template racket/base))

(provide extend-language
         collect-production-identifiers
         build-lang-structs)

;; Find base language, and create new language that extends previous one.
(: extend-language (lang
                    Identifier
                    (U Identifier False)
                    Identifier
                    (Listof non-terminal/delta)
                    -> lang))
(define (extend-language orig name sname entry non-terms)
  (match orig
    [(lang name* sname* entry* non-terminals*)
     (lang name sname entry
           (extend-non-terminals non-terminals* non-terms))]))

;; Given previous list of non-terminals, create list of non-terminals for new language.
(: extend-non-terminals ((Listof non-terminal) (Listof non-terminal/delta) -> (Listof non-terminal)))
(define (extend-non-terminals non-terms deltas)
  (for*/list : (Listof non-terminal)
             ([non-term (in-list non-terms)]
              [delta (in-list deltas)]
              #:when (free-identifier=? (non-terminal-name non-term)
                                        (non-terminal/delta-name delta)))
    (extend-non-terminal non-term delta)))

; Like extend-non-terminal, but only extends one non-terminal.
(: extend-non-terminal (non-terminal non-terminal/delta -> non-terminal))
(define (extend-non-terminal non-term delta)
  (match* (non-term delta)
    [((non-terminal name sname productions) (non-terminal/delta name* +prod -prod))
     (non-terminal name*
                   #f
                   (for/list : (Listof production)
                             ([p (in-list
                                  (append +prod
                                          (remove* -prod
                                                   (map production-pattern productions)
                                                   (lambda ([x : Syntax] [y : Syntax])
                                                     (equal? (syntax->datum x)
                                                             (syntax->datum y))))))])
                     (production #f #f null p)))]))

;; Collect identifiers in production rules.
;; These are turned into structs.
(: collect-production-identifiers (lang -> (Setof Identifier)))
(define (collect-production-identifiers language)
  (match language
    [(lang name sname entry non-terms)
     (immutable-free-id-set
      (map non-terminal-name non-terms))]))

;; Generate structs for a language.
;; Each non-terminal get's a struct, as well as each production.
(: build-lang-structs (lang (Syntaxof Any) -> (Syntaxof Any)))
(define (build-lang-structs language stx)
  (match language
    [(lang name sname entry non-terminals)
     #`((struct #,sname () #:prefab)
        #,@(for/list : (Listof (Syntaxof Any)) ([non-t (in-list non-terminals)])
             (match non-t
               [(non-terminal non-t-name non-t-sname productions)
                #`(begin
                    (struct #,non-t-sname #,sname () #:prefab)
                    #,@(for/list : (Listof (Syntaxof Any)) ([rule (in-list productions)])
                         (match rule
                           [(production name** sname** fields** pattern**)
                            #`(struct #,sname** #,non-t-sname
                                #,(for/list : (Listof (Syntaxof Any)) ([f (in-list fields**)])
                                    (production-field-structname f))
                                #:prefab)])))])))]))
