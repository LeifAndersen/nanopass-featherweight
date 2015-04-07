#lang scribble/manual
@require[@for-label[nanopass-featherweight
                    racket/base]]

@title{A Featherweight Nanopass}
@author{leif}

@defmodule[nanopass-featherweight]

A featherweight implementation of a nanopass framework.

Defines 3 forms:
@itemlist[
@item{@racket[define-language]}
@item{@racket[define-pass]}
@item{@racket[define-compiler]}
]