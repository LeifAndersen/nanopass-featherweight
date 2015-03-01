#lang typed/racket/base/no-check

(provide (struct-out lang)
         (struct-out terminal)
         (struct-out non-terminal)
         (struct-out non-terminal/delta)
         (struct-out lang-symb))

(struct lang ([name : Identifier]
              [entry : (U Identifier False)]
              [terminals : (Listof terminal)]
              [non-terminals : (Listof non-terminal)])
  #:transparent)

(struct terminal ([pred : Identifier];(-> Any Boolean)]
                  [names : (Listof Identifier)])
  #:transparent)

(struct non-terminal ([name : Identifier]
                      [alts : (Listof Identifier)]
                      [productions : (Listof Syntax)])
  #:transparent)

(struct non-terminal/delta ([name : Identifier]
                            [+prod : (Listof Syntax)]
                            [-prod : (Listof Syntax)])
  #:transparent)

(struct lang-symb ([type : Symbol]
                   [name : (U Symbol False)])
  #:transparent)
