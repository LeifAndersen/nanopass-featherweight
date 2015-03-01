#lang typed/racket/base/no-check

(require syntax/parse
         ;racket/syntax
         racket/match
         racket/list
         (only-in racket/set
                  set
                  set-union)
         "structs.rkt"
         (for-template racket/base))

(provide extend-language
         extend-non-terminals
         symb-split
         collect-production-identifiers)

(: extend-language (lang Identifier (U Identifier False)
                    (Listof terminal)
                    (Listof terminal)
                    (Listof non-terminal/delta)
                    -> lang))
(define (extend-language orig name entry +terms -terms non-terms)
  (match orig
    [(lang name* entry* terminals* non-terminals*)
     (lang name entry
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
    [((non-terminal name alts productions) (non-terminal/delta name* +alts +prod -prod))
     (non-terminal name* (append +alts alts)
                   (append +prod (remove* -prod productions)))]))

(: collect-production-identifiers (lang -> (Setof Symbol))) ; wish was -> (Setof Identifier)
(define (collect-production-identifiers language)
  (match language
    [(lang name entry terms non-terms)
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
          [(non-terminal name alts productiosn)
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
