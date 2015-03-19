#lang typed/racket/base/no-check

(provide (struct-out lang)
         (struct-out terminal)
         (struct-out non-terminal)
         (struct-out non-terminal/delta)
         (struct-out production)
         (struct-out lang-symb)
         (struct-out pass)
         (struct-out processor))

;;
;; Language structs
;;

(struct lang ([name : Identifier]
              [structname : (U Identifier False)]
              [entry : Identifier]
              [terminals : (Listof terminal)]
              [non-terminals : (Listof non-terminal)])
  #:transparent)

(struct terminal ([pred : Identifier];(-> Any Boolean)]
                  [names : (Listof Identifier)])
  #:transparent)

(struct non-terminal ([name : Identifier]
                      [structname : (U Identifier False)]
                      [alts : (Listof Identifier)]
                      [productions : (Listof production)]
                      [parser : (U False (Syntax -> Syntax))])
  #:transparent)

(struct production ([name : (U Identifier False)]
                    [structname : (U Identifier False)]
                    [fields : (Listof Identifier)]
                    [pattern : Syntax])
  #:transparent)

(struct non-terminal/delta ([name : Identifier]
                            [alts : (Listof Identifier)]
                            [+prod : (Listof Syntax)]
                            [-prod : (Listof Syntax)])
  #:transparent)

;;
;; Production Structs
;;

(struct lang-symb ([type : Symbol]
                   [name : (U Symbol False)])
  #:transparent)

;;
;; Pass Structs
;;

(struct pass ([name : Identifier]
              [ilang : lang]
              [olang : lang]
              [formals : (Listof Identifier)]
              [returns : (Listof Identifier)]
              [processors : (Listof processor)]
              [definitions : (Listof Syntax)]
              [body : Syntax])
  #:transparent)

(struct processor ([name : Identifier]
                   [int : non-terminal]
                   [ont : non-terminal]
                   [formals : (Listof Identifier)]
                   [returns : (Listof Identifier)]
                   [type : (U 'pattern 'body)]
                   [body : (U (Listof Syntax) Syntax)])
  #:transparent)
