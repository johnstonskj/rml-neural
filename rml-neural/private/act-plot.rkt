#lang racket/base
;;
;; rml-neural - rml-neural/private/act-struct.
;;   
;;
;; Copyright (c) 2019 Simon Johnston (simonjo@amazon.com).

(provide plot-sample)

;; ---------- Requirements

(require plot
         rml-neural/private/act-struct)

;; ---------- Implementation
  
(define (plot-sample af #:from [from -10] #:to [to 10] #:out-file [out-file #f])
  (define Φ (activator-f af))
  (define dΦ (activator-df af))
  (plot (list (axes)
              (function Φ from to #:color "Firebrick")
              (function dΦ from to #:color "Turquoise"))
        #:title (format "f(x) = ~a" (activator-name af))
        #:out-file out-file
        #:out-kind 'png))
