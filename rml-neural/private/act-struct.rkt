#lang racket/base
;;
;; rml-neural - rml-neural/private/act-struct.
;;   
;;
;; Copyright (c) 2019 Simon Johnston (simonjo@amazon.com).

(require racket/contract)

(provide
 real/c
 real-activation/c
 
 (except-out (struct-out activator)
             activator)
 
 flonum/c
 flonum-activation/c

 (except-out (struct-out flonum-activator)
             flonum-activator)

 (contract-out
  [make-activator
   (->* (symbol? real-activation/c real-activation/c) (real/c) activator?)]
  
  [make-flonum-activator
   (->* (symbol? flonum-activation/c flonum-activation/c) (flonum/c) flonum-activator?)]))

;; ---------- Implementation Contracts

(require rml-neural/private/contract)

(define-datatype-contract real)
(define-function1-contract real activation)

(define-datatype-contract flonum)
(define-function1-contract flonum activation)

;; ---------- Implementation Structs

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

