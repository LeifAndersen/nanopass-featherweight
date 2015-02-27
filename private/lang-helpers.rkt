#lang typed/racket/base/no-check

(require syntax/parse
         racket/syntax
         "structs.rkt"
         (for-template racket/base))

(provide extend-language
         extend-non-terminals
         build-lang-structs)

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
              #:when (equal? (non-terminal-name non-term)
                             (non-terminal/delta-name delta)))
    (extend-non-terminal non-term delta)))

(: extend-non-terminal (non-terminal non-terminal/delta -> non-terminal))
(define (extend-non-terminal non-term delta)
  (match* (non-term delta)
    [((non-terminal name alts productions) (non-terminal/delta name +prod -prod))
     (non-terminal name alts (append +prod (remove* -prod productions)))]))

(: build-lang-structs (lang Syntax -> Syntax))
(define (build-lang-structs language stx)
  (match language
    [(lang name entry terminals non-terminals)
     (define name* (format-id stx "~a-struct" name))
     #`((struct #,name* ())
        #,@(for/list ([non-t (in-list non-terminals)])
             (match non-t
               [(non-terminal non-t-name alts productions)
                (define non-t-name* (format-id stx "~a:~a" name non-t-name))
                #`(struct #,non-t-name* #,name* ())])))]))

(: collect-productions (lang -> (Setof Identifier)))
(define (collect-productions lang)
  (set))
