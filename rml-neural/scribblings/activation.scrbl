#lang scribble/manual

@(require racket/sandbox
          scribble/core
          scribble/eval
          scribble-math
          rml-neural
          (for-label racket/base
                     racket/contract
                     rml-neural
                     rml-neural/activation))

@;{============================================================================}

@(define example-eval (make-base-eval
                      '(require racket/string
                                rml-neural
                                rml-neural/activation)))

@;{============================================================================}

@title[]{Module rml-neural/activation.}
@defmodule[rml-neural/activation]

This module defines a set of @italic{activation functions}, or @italic{method}
that may be used to determine the sensitivity of neurons in a network layer.
To support both forward and backward propagation each method contains the
activation function and it's derivative. This
@hyperlink["https://en.wikipedia.org/wiki/Activation_function"]{Wikipedia}
page has a good overview of a number of activation functions.

@(use-mathjax)

@section{Activation Function Structure}

@deftogether[(
              @defthing[maybe-real/c flat-contract?]
               @defthing[maybe-flonum/c flat-contract?])]{
Contracts that encapsulate the pattern @italic{data-type or false}.
}

@deftogether[(
              @defthing[real-activation/c flat-contract?]
               @defthing[flonum-activation/c flat-contract?])]{
Contracts used to define the procedures used in the structures below. Both
activation and derivative functions are represented as a procedure that take
a single, and return a single, @racket[real?] or @racket[flonum?]. They are
equivalent to the following contract values.

@racketblock[
(-> real? real?)
(-> flonum? flonum?)
]

@margin-note*{See also @secref["effective-futures" #:doc '(lib "scribblings/guide/guide.scrbl")]
in @other-doc['(lib "scribblings/guide/guide.scrbl")]} In general it
is preferable to use the @racket[flonum-activator?] structure and the
corresponding @racket[flonum-activation/c] form as this reduces the numeric
conversions and allows optimization such as futures to work efficiently.
}

@defstruct*[activator
            ([name symbol?]
             [f real-activation/c]
             [df real-activation/c]
             [α maybe-real/c])]{
This structure provides the activator function, it's derivative, and an optional
@italic{expectation value} for a given method.

@itemlist[
 @item{@racket[f] is the activation function, @${\phi(v_i)}}
 @item{@racket[df] is the activation derivative function, @${\phi^\prime(v_i)}
   -- sometimes shown as @${\phi^{-1}(v_i)}}
 @item{@racket[α] is an optional stochastic variable sampled from a uniform
   distribution at training time and fixed to the @italic{expectation value}
   of the distribution at test time}]

}

@defstruct*[(flonum-activator activator)
            ([name symbol?]
             [f flonum-activation/c]
             [df flonum-activation/c]
             [α maybe-flonum/c])]{
An extension to @racket[activator?] that ensures that @bold{all} values to 
the functions @racket[f] and @racket[f] as well as the value for @racket[α]
are guaranteed to be @racket[flonum?]s. @margin-note*{See also
 @secref["fixnums+flonums" #:doc '(lib "scribblings/guide/guide.scrbl")]
in @other-doc['(lib "scribblings/guide/guide.scrbl")].} This allows for additional optimization
and all math operations will be assumed to be @racket[flonum] safe.
}

@deftogether[(
 @defproc[(make-activator
           [name symbol?]
           [f real-activation/c]
           [df real-activation/c]
           [α maybe-real/c #f])
          activator?]
 @defproc[(make-flonum-activator
           [name symbol?]
           [f flonum-activation/c]
           [df flonum-activation/c]
           [α maybe-flonum/c #f])
          flonum-activator?])]{
Construct an instance of @racket[activator?] and @racket[flonum-activator?]
respectively. These constructors makes the value for @racket[α] explicitly
optional.
}

@section{Activation Functions}

Each of the @racket[activator?] structures below will be defined by it's
activation function (the derivative is not shown). A sample plot shows the
shape of the activation function in red and it's derivative in turquoise.

@deftogether[(
              @defthing[flidentity flonum-activator?]
              @defthing[identity activator?])]{
@$${\phi(v_i) = v_i}

@centered{
  @image["scribblings/act-identity->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defthing[flbinary-step flonum-activator?]
               @defthing[binary-step activator?])]{
@$${\phi(v_i) =
  \begin{cases}
    0       & \text{for } v_i < 0\\
    1       & \text{for } v_i \geq 0
  \end{cases}}

@centered{
  @image["scribblings/act-binary-step->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defthing[flsigmoid flonum-activator?]
               @defthing[sigmoid activator?])]{
@$${\phi(v_i) = \frac{1}{1+e^{-v_i}}}

@centered{
  @image["scribblings/act-sigmoid->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defthing[fltanh flonum-activator?]
               @defthing[tanh activator?])]{
@$${\phi(v_i) = \tanh(v_i)}

@centered{
  @image["scribblings/act-tanh->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defthing[flarc-tan flonum-activator?]
               @defthing[arc-tan activator?])]{
@$${\phi(v_i) = \operatorname{atan}^{-1}(v_i)}

@centered{
  @image["scribblings/act-arc-tan->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defthing[flelliot-sigmoid flonum-activator?]
               @defthing[elliot-sigmoid activator?])]{
@$${\phi(v_i) = \frac{v_i}{1+\left|v_i\right|}}

@centered{
  @image["scribblings/act-elliot-sigmoid->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defproc[(flinverse-square-root-unit
                        [α flonum?])
                       flonum-activator?]
               @defproc[(inverse-square-root-unit
                         [α number?])
                        activator?])]{
@$${\phi(v_i) = \frac{v_i}{\sqrt{1+\alpha v_{i}^2}}}

@centered{
  @image["scribblings/act-inverse-square-root-unit->flonum.png" #:scale 0.5]{Sample Plot (α = 0.5)}}
}

@deftogether[(
              @defproc[(flinverse-square-root-linear-unit
                        [α flonum?])
                       flonum-activator?]
               @defproc[(inverse-square-root-linear-unit
                        [α number?])
                       activator?])]{
@$${\phi(v_i) =
  \begin{cases}
    \frac{v_i}{\sqrt{1+\alpha v_{i}^2}}  & \text{for } v_i < 0\\
    v_i       & \text{for } v_i \geq 0
  \end{cases}}

@centered{
  @image["scribblings/act-inverse-square-root-linear-unit->flonum.png" #:scale 0.5]{Sample Plot (α = 0.5)}}
}

@deftogether[(
              @defthing[flrectified-linear-unit flonum-activator?]
               @defthing[rectified-linear-unit activator?])]{
@$${\phi(v_i) =
  \begin{cases}
    0       & \text{for } v_i < 0\\
    v_i     & \text{for } v_i \geq 0
  \end{cases}}

@centered{
  @image["scribblings/act-rectified-linear-unit->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defproc[(flleaky-rectified-linear-unit [∂ flonum?]) flonum-activator?]
              @defthing[flfixed-leaky-rectified-linear-unit flonum-activator?]
               @defthing[fixed-leaky-rectified-linear-unit activator?])]{
@$${\phi(v_i) =
  \begin{cases}
    \delta v_i  & \text{for } v_i < 0\\
    v_i         & \text{for } v_i \geq 0
  \end{cases}}

@centered{
  @image["scribblings/act-fixed-leaky-rectified-linear-unit->flonum.png" #:scale 0.5]{Sample Plot}}


Note that the @italic{fixed} form of this activator uses a delta value @${\delta=0.01}.
}

@deftogether[(
              @defthing[flsoftplus flonum-activator?]
               @defthing[softplus activator?])]{
@$${\phi(v_i) = \ln\left( 1 + e^{v_i} \right)}

@centered{
  @image["scribblings/act-softplus->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defthing[flbent-identity flonum-activator?]
               @defthing[bent-identity activator?])]{
@$${\phi(v_i) = \frac{\sqrt{v_{i}^2+1}-1}{2}+v_i}

@centered{
  @image["scribblings/act-bent-identity->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defthing[flsinusoid flonum-activator?]
               @defthing[sinusoid activator?])]{
@$${\phi(v_i) = \sin(v_i)}

@centered{
  @image["scribblings/act-sinusoid->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defthing[flsinc flonum-activator?]
               @defthing[sinc activator?])]{
@$${\phi(v_i) =
  \begin{cases}
    1                      & \text{for } v_i = 1\\
    \frac{\sin(v_i)}{v_i}  & \text{for } v_i \neq 0
  \end{cases}}

@centered{
  @image["scribblings/act-sinc->flonum.png" #:scale 0.5]{Sample Plot}}
}

@deftogether[(
              @defthing[flgaussian flonum-activator?]
               @defthing[gaussian activator?])]{
@$${\phi(v_i) = e^{-v_{i}^2}}

@centered{
  @image["scribblings/act-gaussian->flonum.png" #:scale 0.5]{Sample Plot}}
}

