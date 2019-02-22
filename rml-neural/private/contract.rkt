#lang racket/base
;;
;; rml-neural - rml-neural/private/contract.
;;   
;;
;; Copyright (c) 2019 Simon Johnston (simonjo@amazon.com).


(provide
 (all-defined-out))

;; ---------- Requirements

(require racket/bool
         racket/contract
         (for-syntax
          racket/base
          racket/struct
          racket/syntax))

;; ---------- Internal types

;; ---------- Implementation

(define-syntax (define-datatype-contract stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([name (format-id #'id "~a/c" #'id)]
                   [predicate (format-id #'id "~a?" #'id)])
       #`(define name
           (make-flat-contract #:name (quote name)
                               #:first-order predicate)))]))

(define-syntax (define-optional-datatype-contract stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([name (format-id #'id "maybe-~a/c" #'id)]
                   [predicate (format-id #'id "~a?" #'id)])
       #`(define name
           (make-flat-contract #:name (quote name)
                               #:first-order (λ (v) (or (predicate v) (false? v))))))]))

(define-syntax (define-function1-contract stx)
  (syntax-case stx ()
    [(_ id suffix)
     (and (identifier? #'id) (identifier? #'suffix))
     (with-syntax ([name (format-id #'id "~a-~a/c" #'id #'suffix)]
                   [type-contract (format-id #'id "~a/c" #'id)]
                   [expect-str (format "a function (~a? . -> . ~a?)"
                                       (syntax->datum #'id)
                                       (syntax->datum #'id))])
       #`(define name
           (make-contract
            #:name (quote name)
            #:first-order
            (λ (x) (and (procedure? x) (procedure-arity-includes? x 1)))
            #:projection
            (λ (b)
              (let ([domain ((contract-projection type-contract) (blame-swap b))]
                    [range ((contract-projection type-contract) b)])
                (λ (f)
                  (if (and (procedure? f) (procedure-arity-includes? f 1))
                      (λ (x) (range (f (domain x))))
                      (raise-blame-error
                       b f
                       '(expected expect-str given: "~e")
                       f))))))))]))
