#lang typed/racket/base/no-check

(provide make-immutable-identifier-set
         make-mutable-identifier-set
         make-weak-identifier-set
         identifier-set?
         immutable-identifier-set?
         mutable-identifier-set?
         weak-identifier-set?)

(define-custom-set-types identifier-set
  #:elem? identifier?
  free-identifier=?)
