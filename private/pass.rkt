#lang racket/base

(require (for-syntax syntax/parse
                     racket/syntax
                     racket/base
                     racket/pretty
                     "structs.rkt"
                     "pass-helpers.rkt"))
(provide define-pass)

(begin-for-syntax
  (define-syntax-class proc
    (pattern (name:id (~datum :) (~or NTin-name:id #f) (~datum ->) (~or NTout-name:id #f)
                      (~optional (~seq #:formals (formals:id ...))
                                 #:defaults ([(formals 1) (list #'e)]))
                      (~optional (~seq #:returns (returns:id ...))
                                 #:defaults ([(returns 1) null]))
                      rules ...))))

(define-syntax (define-pass stx)
  (syntax-parse stx
    [(_ name:id (~datum :) (~or Li-name:id #f) (~datum ->) (~or Lo-name:id #f)
        (~optional (~seq #:formals (formals:id ...))
                   #:defaults ([(formals 1) (list #'e)]))
        (~optional (~seq #:return (returns:id ...))
                   #:defaults ([(returns 1) null]))
        (~optional (~seq #:definitions (definitions ...))
                   #:defaults ([(definitions 1) null]))
        (~optional (~seq #:body body)
                   #:defaults ([body #f]))
        proc:proc ...)
     (define Li (lookup-lang (attribute Li-name)))
     (define Lo (lookup-lang (attribute Lo-name)))
     (define body* (build-body Li
                               (attribute formals)
                               (attribute body)))
     (define p (pass (attribute name)
                     Li Lo
                     (attribute formals)
                     (attribute formals)
                     (for/list ([i (in-list (attribute proc))])
                       (proc->proccessor i Li Lo))
                     (attribute definitions)
                     body*))
     (with-syntax ([(processors ...) (build-processors stx p)]
                   [body** body*])
       (syntax/loc stx
         (define (name formals ...)
           definitions ...
           processors ...
           body**)))]))

(define-for-syntax (proc->proccessor stx Li Lo)
  (syntax-parse stx
    [proc:proc
     (processor (format-id (lang-name Li) "~a" (syntax-e (attribute proc.name)))
                (find-nt Li (attribute proc.NTin-name))
                (find-nt Lo (attribute proc.NTout-name))
                (attribute proc.formals)
                (attribute proc.returns)
                'pattern
                (attribute proc.rules))]))
