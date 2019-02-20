#lang racket/base
;;
;; rml-neural - rml-neural/scribblings/act-plots.
;;   Neural Networks for Racket
;;
;; Copyright (c) 2019 Simon Johnston (simonjo@amazon.com).

;; see https://en.wikipedia.org/wiki/Activation_function

;; ---------- Requirements

(require racket/flonum
         racket/format
         racket/list
         racket/port
         rml-neural/activation
         rml-neural/private/act-plot)

;; ---------- Implementation

(define methods
  (list identity
        binary-step
        sigmoid
        tanh
        arc-tan
        elliot-sigmoid
        (inverse-square-root-unit 0.5)
        (inverse-square-root-linear-unit 0.5)
        rectified-linear-unit
        fixed-leaky-rectified-linear-unit
        softplus
        bent-identity
        sinusoid
        sinc
        gaussian))
  
(module+ main

  (define base-path
    (path->string (collection-file-path "scribblings" "rml-neural")))
  (displayln (format "Output directory: ~a" base-path))

  (define name-width (apply max (map (λ (a) (string-length (symbol->string (activator-name a)))) methods)))

  ;; warm-up.
  (define ignore-this (call-with-output-bytes
                       (λ (outp)
                         (plot-sample identity #:out-file outp))))
  
  (for ([method methods])
    (define file-name (format "~a/act-~a.png" base-path (activator-name method)))
    (display (format "Generating image for ~a: " (~a (activator-name method) #:width name-width)))
    (let*-values ([(f) (λ (m fn) (plot-sample m #:out-file fn))]
                  [(r c a gc) (time-apply f (list method file-name))])
      (displayln (format "~a | ~a (~a%) | ~a (~a%)"
                         (~a a #:width 6 #:align 'right)
                         (~a c #:width 6 #:align 'right)
                         (~r (* 100.0 (/ c a)) #:precision (list '= 2))
                         (~a gc #:width 6 #:align 'right)
                         (~r (* 100.0 (/ gc a)) #:precision (list '= 2)))))))