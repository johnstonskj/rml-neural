#lang info
;;
;; Package rml-neural.
;;   Neural Networks for Racket
;;
;; Copyright (c) 2019 Simon Johnston (johnstonskj@gmail.com).

(define collection 'multi)

(define pkg-desc "Neural Networks for Racket")
(define version "1.0")
(define pkg-authors '(Simon Johnston))

(define deps '(
  "base"
  "math-lib"
  "plot-gui-lib"
  "plot-lib"
  "rackunit-lib"))
(define build-deps '(
  "scribble-lib"
  "scribble-math"
  "racket-doc"
  "racket-index"
  "sandbox-lib"  
  "cover-coveralls"))
