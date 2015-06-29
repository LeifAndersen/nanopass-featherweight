#lang racket/base

;; Home for define-pass macro.
;;
;; Syntax for define-pass:
;; (define-pass pass-name : In-Lang -> Out-Lang
;;      #:formals [formals (e)]
;;      #:returns  [returns ()]
;;    (processor-name : In-Non-Terminal -> Out-Non-Terminal
;;          #:formals [formals (e)]
;;          #:returns [returns ()]
;;          [pattern template]
;;          ...)
;;    ...)

(provide define-pass)

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/base
                     racket/pretty
                     "structs.rkt"
                     "pass-helpers.rkt"))

;; Syntax class for processors
(begin-for-syntax
  (define-syntax-class proc
    (pattern (name:id (~datum :) (~or NTin-name:id #f) (~datum ->) (~or NTout-name:id #f)
                      (~optional (~seq #:formals (formals:id ...))
                                 #:defaults ([(formals 1) (list #'e)]))
                      (~optional (~seq #:returns (returns ...))
                                 #:defaults ([(returns 1) null]))
                      rules ...))))

;; define-pass macro, syntax at top of file
(define-syntax (define-pass stx)
  (syntax-parse stx
    [(_ name:id (~datum :) (~or Li-name:id #f) (~datum ->) (~or Lo-name:id #f)
        (~optional (~seq #:formals (formals:id ...))
                   #:defaults ([(formals 1) (list #'e)]))
        (~optional (~seq #:return (returns ...))
                   #:defaults ([(returns 1) null]))
        (~optional (~seq #:definitions (definitions ...))
                   #:defaults ([(definitions 1) null]))
        (~optional (~seq #:body body)
                   #:defaults ([body #f]))
        proc:proc ...)
     (define Li (lookup-lang (attribute Li-name)))
     (define Lo (lookup-lang (attribute Lo-name)))
     (define processors (for/list ([i (in-list (attribute proc))])
                          (proc->processor i (attribute name) Li Lo)))
     (define body* (build-body Li
                               processors
                               (attribute formals)
                               (attribute body)))
     (define p (pass (attribute name)
                     Li Lo
                     (attribute formals)
                     (attribute formals)
                     processors
                     (attribute definitions)
                     body*))
     (with-syntax ([(processors ...) (build-processors stx p)]
                   [body** body*])
       (syntax/loc stx
         (define (name formals ...)
           definitions ...
           processors ...
           body**)))]))

;; Turn the processors syntax class into the sturct representation
(define-for-syntax (proc->processor stx pname Li Lo)
  (syntax-parse stx
    [proc:proc
     (processor (format-id (lang-name Li) "~a" (syntax-e (attribute proc.name)))
                (format-id stx "~a:~a" pname (syntax-e (attribute proc.name)))
                (find-nt Li (attribute proc.NTin-name))
                (find-nt Lo (attribute proc.NTout-name))
                (attribute proc.formals)
                (attribute proc.returns)
                'pattern
                (attribute proc.rules))]))
