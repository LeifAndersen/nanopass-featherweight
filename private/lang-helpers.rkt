#lang typed/racket

(require syntax/parse
         "structs.rkt")

(provide extend-language)

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
  non-terms)
