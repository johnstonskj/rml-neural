#lang racket/base
;;
;; rml-neural - rml-neural/private/act-syntax.
;;   
;;
;; Copyright (c) 2019 Simon Johnston (simonjo@amazon.com).


(provide
 (all-defined-out))

;; ---------- Requirements

(require rml-neural/private/act-struct
         (for-syntax
          racket/base
          racket/struct
          racket/syntax
          rml-neural/private/act-struct))

;; ---------- Internal types

;; ---------- Implementation

(define-syntax (define-loose-activator stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([wrapped (format-id #'id "fl~a" #'id)]
                   [name (format-id #'id "~a->flonum" #'id)])
       #`(define id
           (make-activator
            (quote name)
            (λ (x) ((activator-f wrapped) (if (flonum? x) x (real->double-flonum x))))
            (λ (x) ((activator-df wrapped) (if (flonum? x) x (real->double-flonum x)))))))]
    [(_ id var)
     (and (identifier? #'id) (identifier? #'var))
     (with-syntax ([wrapped (format-id #'id "fl~a" #'id)]
                   [a-name (format-id #'id "~a->flonum" #'id)])
       #`(define (id var)
           (define fl-copy (wrapped var))
           (make-activator
            (quote a-name)
            (λ (x) ((activator-f fl-copy) (if (flonum? x) x (real->double-flonum x))))
            (λ (x) ((activator-df fl-copy) (if (flonum? x) x (real->double-flonum x)))))))]))
