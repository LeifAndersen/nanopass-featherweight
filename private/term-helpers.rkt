#lang typed/racket/base/no-check

(provide find-non-terminal
         find-terminal
         find-term-non-term)
(require "structs.rkt")

(: find-non-terminal (lang Identifier -> non-terminal))
(define (find-non-terminal l nt)
  (or (findf (lambda (nt*) (free-identifier=? nt (non-terminal-name nt*)))
                     (lang-non-terminals l))
      (error "Non terminal missing: write error message")))

(: find-terminal (lang Identifier -> terminal))
(define (find-terminal l t)
  (or (findf (lambda (t*) (free-identifier=? t (terminal-pred t*)))
             (lang-terminals l))
      (error "Terminal missing: write error message")))

(: find-term-non-term (lang Identifier -> (U terminal non-terminal)))
(define (find-term-non-term l tnt)
  (or (findf (lambda (nt*) (free-identifier=? tnt (non-terminal-name nt*)))
             (lang-non-terminals l))
      (findf (lambda (t*) (free-identifier=? tnt (terminal-pred t*)))
             (lang-terminals l))
      (error "Terminal/Non Terminal missing: write error message")))

(: find-production (non-terminal Identifier -> production))
(define (find-production nt pr)
  (or (findf (lambda (pr*) (free-identifier=? pr (production-name pr*)))
             (non-terminal-productions nt))
      (error "Production missing: write error message")))
