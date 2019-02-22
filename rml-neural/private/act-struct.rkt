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

(define flonum/c
    (make-flat-contract #:name 'flonum/c #:first-order flonum?))

(define flonum-activation/c
    (make-contract
     #:name 'flonum-activation/c
     #:first-order
     (λ (x) (and (procedure? x) (procedure-arity-includes? x 1)))
     #:projection
     (λ (b)
       (let ([domain ((contract-projection flonum/c) (blame-swap b))]
             [range ((contract-projection flonum/c) b)])
         (λ (f)
           (if (and (procedure? f) (procedure-arity-includes? f 1))
             (λ (x) (range (f (domain x))))
             (raise-blame-error
              b f
              '(expected "a function taking one, and returning one, flonum" given: "~e")
              f)))))))

(define real/c
    (make-flat-contract #:name 'real/c #:first-order real?))

(define real-activation/c
    (make-contract
     #:name 'real-activation/c
     #:first-order
     (λ (x) (and (procedure? x) (procedure-arity-includes? x 1)))
     #:projection
     (λ (b)
       (let ([domain ((contract-projection real/c) (blame-swap b))]
             [range ((contract-projection real/c) b)])
         (λ (f)
           (if (and (procedure? f) (procedure-arity-includes? f 1))
             (λ (x) (range (f (domain x))))
             (raise-blame-error
              b f
              '(expected "a function taking one, and returning one, real" given: "~e")
              f)))))))

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

