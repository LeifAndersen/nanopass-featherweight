#lang typed/racket/base/no-check

;; Structs used to store internal representation of a language.

(provide
 ;; Language
 (struct-out lang)
 (struct-out terminal)
 (struct-out non-terminal)
 (struct-out non-terminal/delta)
 (struct-out production)

 ;; Production Rule
 (struct-out production-field)

 ;; Pass
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
  #:prefab)

(struct terminal ([pred : Identifier];(-> Any Boolean)]
                  [names : (Listof Identifier)])
  #:prefab)

(struct non-terminal ([name : Identifier]
                      [structname : (U Identifier False)]
                      [alts : (Listof Identifier)]
                      [productions : (Listof production)])
  #:prefab)

(struct production ([name : (U Identifier False)]
                    [structname : (U Identifier False)]
                    [fields : (Listof field)]
                    [pattern : Syntax])
  #:prefab)

(struct non-terminal/delta ([name : Identifier]
                            [alts : (Listof Identifier)]
                            [+prod : (Listof Syntax)]
                            [-prod : (Listof Syntax)])
  #:prefab)

;;
;; Production Structs
;;

(struct production-field ([name : Identifier]
                          [structname : Identifier]
                          [depth : Integer])
  #:prefab)

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
  #:prefab)

(struct processor ([name : Identifier]
                   [defname : Identifier]
                   [int : non-terminal]
                   [ont : non-terminal]
                   [formals : (Listof Identifier)]
                   [returns : (Listof Identifier)]
                   [type : (U 'pattern 'body)]
                   [body : (U (Listof Syntax) Syntax)])
  #:prefab)
