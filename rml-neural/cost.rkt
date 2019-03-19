#lang racket/base
;;
;; rml-neural - rml-neural/cost.
;;   Neural Networks for Racket
;;
;; Copyright (c) 2019 Simon Johnston (simonjo@amazon.com).

;; see https://en.wikipedia.org/wiki/Activation_function

(provide
 mean-squared-error
 quadratic-cost)

;; ---------- Requirements

(require racket/flonum
         racket/vector
         math/flonum)

;; ---------- Implementation

(define (mean-squared-error expected actual)
  (define (inner y a) (flsqr (vector-euclidean-length (flvector- y a))))
  (fl* (fl/ 1.0 (fl* 2.0 (fl (vector-length expected))))
       (flvector-sum (vector->flvector (vector-map inner expected actual)))))

(define quadratic-cost mean-squared-error)

;; ---------- Internal procedures

(define (flsqr x) (fl* x x))

(define (vector-euclidean-length v)
  (flsqrt (flvector-sum (flvector-sqr v))))

;; ---------- Internal tests

(module+ test
  (require rackunit)
  
  (check-equal? (mean-squared-error
                 (vector (flvector 1.0))
                 (vector (flvector 0.0)))
                0.5)
  (check-equal? (mean-squared-error
                 (vector (flvector 1.0))
                 (vector (flvector 0.5)))
                0.125)
  (check-equal? (mean-squared-error
                 (vector (flvector 1.0))
                 (vector (flvector 0.8)))
                0.01999999999999999))