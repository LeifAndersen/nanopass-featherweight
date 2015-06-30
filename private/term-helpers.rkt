#lang typed/racket/base/no-check

(provide find-non-terminal)
(require "structs.rkt")

(: find-non-terminal (lang Identifier -> non-terminal))
(define (find-non-terminal l nt)
  (or (findf (lambda (nt*) (free-identifier=? nt (non-terminal-name nt*)))
                     (lang-non-terminals l))
      (error "Non terminal missing: write error message")))

(: find-production (non-terminal Identifier -> production))
(define (find-production nt pr)
  (or (findf (lambda (pr*) (free-identifier=? pr (production-name pr*)))
             (non-terminal-productions nt))
      (error "Production missing: write error message")))
