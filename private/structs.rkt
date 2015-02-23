#lang racket

(provide (struct-out lang)
         (struct-out terminal)
         (struct-out non-terminal)
         (struct-out non-terminal/delta))

(struct lang (name;[name : Identifier]
              entry;[entry : (U Identifier False)]
              terminals;[terminals : (Listof terminal)]
              non-terminals #;[non-terminals : (Listof non-terminal)])
  #:prefab)

(struct terminal (pred;[pred : Identifier];(-> Any Boolean)]
                  names #;[names : (Listof Identifier)])
  #:prefab)

(struct non-terminal (name ;[name : Identifier]
                      alts ;[alts : (Listof Identifier)]
                      productions #;[productions : (Listof Any)])
  #:prefab)

(struct non-terminal/delta (name ;[name : Identifier]
                            +prod ;[+prod : (Listof Any)]
                            -prod #;[-prod : (Listof Any)])
  #:prefab)
