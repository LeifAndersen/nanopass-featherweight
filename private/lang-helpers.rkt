#lang typed/racket/base/no-check

(require syntax/parse
         racket/match
         racket/list
         unstable/syntax
         (only-in racket/set
                  set
                  set-union)
         "structs.rkt"
         (for-template racket/base))

(provide extend-language
         symb-split
         collect-production-identifiers
         build-lang-structs)

(: extend-language (lang
                    Identifier
                    (U Identifier False)
                    Identifier
                    (Listof terminal)
                    (Listof terminal)
                    (Listof non-terminal/delta)
                    -> lang))
(define (extend-language orig name sname entry +terms -terms non-terms)
  (match orig
    [(lang name* sname* entry* terminals* non-terminals*)
     (lang name sname entry
           (append +terms (remove* -terms terminals*))
           (extend-non-terminals non-terminals* non-terms))]))

(: extend-non-terminals ((Listof non-terminal) (Listof non-terminal/delta) -> (Listof non-terminal)))
(define (extend-non-terminals non-terms deltas)
  (for*/list : (Listof non-terminal)
             ([non-term (in-list non-terms)]
              [delta (in-list deltas)]
              #:when (free-identifier=? (non-terminal-name non-term)
                                        (non-terminal/delta-name delta)))
    (extend-non-terminal non-term delta)))

(: extend-non-terminal (non-terminal non-terminal/delta -> non-terminal))
(define (extend-non-terminal non-term delta)
  (match* (non-term delta)
    [((non-terminal name sname alts productions parser) (non-terminal/delta name* +alts +prod -prod))
     (non-terminal name*
                   #f
                   (append +alts alts)
                   (append (for/list : (Listof production) ([p (in-list +prod)])
                             (production #f #f null p))
                           (remove* (for/list : (Listof production) ([p (in-list -prod)])
                                      (production #f #f null p))
                                    productions)))]))

(: collect-production-identifiers (lang -> (Setof Symbol))) ; wish was -> (Setof Identifier)
(define (collect-production-identifiers language)
  (match language
    [(lang name sname entry terms non-terms)
     (set-union
      (for/fold : (Setof Symbol) ([acc : (Setof Symbol) (set)])
                                 ([i (in-list terms)])
        (match i
          [(terminal pred names)
           (set-union acc
                      (for/set : (Setof Symbol) ([j (in-list names)]) (syntax-e j)))]))
      (for/fold : (Setof Symbol) ([acc : (Setof Symbol) (set)])
                                 ([i (in-list non-terms)])
        (match i
          [(non-terminal name sname alts productions parser)
           (set-union acc
                      (for/set : (Setof Symbol) ([j (in-list alts)])
                        (lang-symb-type (symb-split (syntax-e j)))))])))]))

(: symb-split (Symbol -> lang-symb))
(define (symb-split symb)
  (define symb* (regexp-match "([^_]*)(_(.*))?" (symbol->string symb)))
  (match symb*
    [(list _ s _ f)
     #:when (string? s)
     (lang-symb (string->symbol s)
                (if (string? f)
                    (string->symbol f)
                    #f))]))

(: build-lang-structs (lang (Syntaxof Any) -> (Syntaxof Any)))
(define (build-lang-structs language stx)
  (match language
    [(lang name sname entry terminals non-terminals)
     #`((struct #,sname ())
        #,@(for/list : (Listof (Syntaxof Any)) ([non-t (in-list non-terminals)])
             (match non-t
               [(non-terminal non-t-name non-t-sname alts productions parser)
                #`(begin
                    (struct #,non-t-sname #,sname ())
                    #,@(for/list : (Listof (Syntaxof Any)) ([rule (in-list productions)])
                         (match rule
                           [(production name** sname** fields** pattern**)
                            #`(struct #,sname** #,non-t-sname (#,@fields**))])))])))]))
