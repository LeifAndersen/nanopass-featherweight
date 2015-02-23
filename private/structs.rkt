#lang typed/racket

(provide lang terminal non-terminal non-terminal/delta)

(struct lang ([name : Identifier]
              [entry : (U Identifier False)]
              [terminals : (Listof terminal)]
              [non-terminals : (Listof non-terminal)]))

(struct terminal ([pred : Identifier];(-> Any Boolean)]
                  [names : (Listof Identifier)]))

(struct non-terminal ([name : Identifier]
                      [alts : (Listof Symbol)]
                      [productions : (Listof Any)]))

(struct non-terminal/delta ([name : Identifier]
                            [+prod : (Listof Any)]
                            [-prod : (Listof Any)]))
