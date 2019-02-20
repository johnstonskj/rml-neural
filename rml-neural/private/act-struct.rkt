#lang racket/base
;;
;; rml-neural - rml-neural/private/act-struct.
;;   
;;
;; Copyright (c) 2019 Simon Johnston (simonjo@amazon.com).

(provide
 (except-out (struct-out activator)
             activator)
 make-activator
 (except-out (struct-out flonum-activator)
             flonum-activator)
 make-flonum-activator)

;; ---------- Implementation

(struct activator
  (name
   f
   df
   α))

(struct flonum-activator activator ())

(define (make-activator name f df [α #f])
  (activator name f df α))

(define (make-flonum-activator name f df [α #f])
  (flonum-activator name f df α))

