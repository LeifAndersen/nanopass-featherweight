#lang typed/racket/base/no-check

(require racket/match
         "structs.rkt")

(provide build-processors
         find-nt
         build-body
         lookup-lang)

(define (build-processors stx)
  #'(void))

(define (build-processor stx)
  #'(void))

(: find-nt (lang Identifier -> non-terminal))
(define (find-nt language nt)
  (match language
    [(lang name entry terminals non-terminals)
     (findf (lambda (x) (free-identifier=? nt x)) non-terminals)]))

(: build-body (lang (Listof Syntax) (U Syntax False) -> Syntax))
(define (build-body language formals body)
  (or body
      (match language
        [(lang name entry terminals non-terminals)
         #`(#,entry #,(first formals))])))

(: lookup-lang (Identifier -> (U lang False)))
(define (lookup-lang lang)
  (cond [(identifier? lang)
         (syntax-local-value lang)]
        [else #f]))
