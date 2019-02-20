#lang racket/base
;;
;; rml-neural - rml-neural/activation.
;;   Neural Networks for Racket
;;
;; Copyright (c) 2019 Simon Johnston (simonjo@amazon.com).

;; see https://en.wikipedia.org/wiki/Activation_function

(provide
 
 (all-from-out rml-neural/private/act-struct)
 
 flidentity
 identity
 flbinary-step
 binary-step
 flsigmoid
 sigmoid
 fltanh
 tanh
 flarc-tan
 arc-tan
 flelliot-sigmoid
 elliot-sigmoid
 flinverse-square-root-unit
 inverse-square-root-unit
 flinverse-square-root-linear-unit
 inverse-square-root-linear-unit
 flrectified-linear-unit
 rectified-linear-unit
 flleaky-rectified-linear-unit
 flfixed-leaky-rectified-linear-unit
 fixed-leaky-rectified-linear-unit
 flsoftplus
 softplus
 flbent-identity
 bent-identity
 flsinusoid
 sinusoid
 flsinc
 sinc
 flgaussian
 gaussian)

;; ---------- Requirements

(require racket/flonum
         ;math/flonum   ;; fltanh is painfully slow!
         (prefix-in math: racket/math)
         rml-neural/private/act-struct
         rml-neural/private/act-syntax)

;; ---------- Implementation

(define flidentity
  (make-flonum-activator
   'identity
   (λ (x) x)
   (λ (x) 1.0)))

(define-loose-activator identity)

(define flbinary-step
  (make-flonum-activator
   'binary-step
   (λ (x) (cond
            [(fl< x 0.0) 0.0]
            [else 1.0]))
   (λ (x) (cond
            [(fl= x 0.0) (error "binary step activation function "
                                "is not differentiable at 0")]
            [else 0.0]))))

(define-loose-activator binary-step)

(define flsigmoid
  (let ([σ (λ (x) (fl/ 1.0
                       (fl+ 1.0 (flexp (fl- x)))))])
    (make-flonum-activator
     'sigmoid
     σ
     (λ (x) (let ([sx (σ x)]) ; just the once
              (fl* sx (fl- 1.0 sx)))))))

(define-loose-activator sigmoid)

(define fltanh
  (make-flonum-activator
   'tanh
   math:tanh
   (λ (x) (fl- 1.0 (flexpt (math:tanh x) 2.0)))))

(define-loose-activator tanh)

(define flarc-tan
  (make-flonum-activator
   'arc-tan
   (λ (x) (flatan x))
   (λ (x) (fl/ 1.0
               (fl+ (flexpt x 2.0) 1.0)))))

(define-loose-activator arc-tan)

(define flelliot-sigmoid
  (make-flonum-activator
   'elliot-sigmoid
   (λ (x) (fl/ x
               (fl+ 1.0 (flabs x))))
   (λ (x) (fl/ 1.0
               (fl+ (flexpt x 2.0) 1.0)))))

(define-loose-activator elliot-sigmoid)

(define (flinverse-square-root-unit α)
  (make-flonum-activator
   'inverse-square-root-unit
   (λ (x) (fl/ x
               (flsqrt (fl+ 1.0 (fl* α (flexpt x 2.0))))))
   (λ (x) (flexpt (fl/ 1.0
                       (flsqrt (fl+ 1.0 (fl* α (flexpt x 2.0)))))
                  3.0))
   α))

(define-loose-activator inverse-square-root-unit α)

(define (flinverse-square-root-linear-unit α)
  (define isru (flinverse-square-root-unit α))
  (make-flonum-activator
   'inverse-square-root-linear-unit
   (λ (x) (cond
            [(fl< x 0.0) ((activator-f isru) x)]
            [else x]))
   (λ (x) (cond
            [(fl< x 0.0) ((activator-df isru) x)]
            [else 1.0]))
   α))

(define-loose-activator inverse-square-root-linear-unit α)

(define flrectified-linear-unit
  (make-flonum-activator
   'rectified-linear-unit
   (λ (x) (cond
            [(fl< x 0.0) 0.0]
            [else x]))
   (λ (x) (cond
            [(fl< x 0.0) 0.0]
            [else 1.0]))))

(define-loose-activator rectified-linear-unit)

(define (flleaky-rectified-linear-unit ∂)
  (make-flonum-activator
   'fixed-leaky-rectified-linear-unit
   (λ (x) (cond
            [(fl< x 0.0) (fl* ∂ x)]
            [else x]))
   (λ (x) (cond
            [(fl< x ∂) 0.0]
            [else 1.0]))))

(define flfixed-leaky-rectified-linear-unit
  (flleaky-rectified-linear-unit 0.01))

(define-loose-activator fixed-leaky-rectified-linear-unit)

(define flsoftplus
  (make-flonum-activator
   'softplus
   (λ (x) (fllog (fl+ 1.0 (flexp x))))
   (λ (x) (fl/ 1.0
               (fl+ 1.0 (flexp (fl- x)))))))

(define-loose-activator softplus)

(define flbent-identity
  (make-flonum-activator
   'bent-identity
   (λ (x) (fl+ (fl/ (fl- (flsqrt (fl+ (flexpt x 2.0) 1.0)) 1.0)
                    2.0)
               x))
   (λ (x) (fl+ (fl/ x
                    (fl* 2.0 (flsqrt (fl+ (flexpt x 2.0) 1.0))))
               1.0))))

(define-loose-activator bent-identity)

(define flsinusoid
  (make-flonum-activator
   'sinusoid
   flsin
   flcos))

(define-loose-activator sinusoid)

(define flsinc
  (make-flonum-activator
   'sinc
   (λ (x) (cond
            [(fl= x 0.0) 1.0]
            [else (fl/ (flsin x)
                       x)]))
   (λ (x) (cond
            [(fl= x 0.0) 0.0]
            [else (fl- (fl/ (flcos x)
                            x)
                       (fl/ (flsin x)
                            (flexpt x 2.0)))]))))

(define-loose-activator sinc)

(define flgaussian 
  (let ([f (λ (x) (flexp (fl- (flexpt x 2.0))))])
    (make-flonum-activator
     'gaussian
     f
     (λ (x) (fl* (fl- 2.0) x (f x))))))

(define-loose-activator gaussian)

