#lang racket/base
;;
;; rml-neural - rml-neural.
;;   
;;
;; Copyright (c) 2019 Simon Johnston (simonjo@amazon.com).

;; Racket Style Guide: http://docs.racket-lang.org/style/index.html

(require racket/contract)

(provide
 (contract-out))

;; ---------- Requirements

(require "private/rml-neural.rkt")

;; ---------- Internal types

;; ---------- Implementation

;; ---------- Internal procedures

;; ---------- Internal tests


(module+ test
  (require rackunit)
  ;; only use for internal tests, use check- functions 
  (check-true "dummy first test" #f))


(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
