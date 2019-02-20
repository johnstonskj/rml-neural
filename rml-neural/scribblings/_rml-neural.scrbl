#lang scribble/manual

@(require racket/sandbox
          scribble/core
          scribble/eval
          rml-neural
          (for-label racket/base
                     racket/contract
                     rml-neural))

@;{============================================================================}

@(define example-eval (make-base-eval
                      '(require racket/string
                                rml-neural)))

@;{============================================================================}

@title[]{Module rml-neural.}
@defmodule[rml-neural]



@examples[ #:eval example-eval
(require rml-neural)
; add more here.
]

@;{============================================================================}

@;Add your API documentation here...


Document  - TBD
