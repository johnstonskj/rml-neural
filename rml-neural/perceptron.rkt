#lang racket/base
;;
;; rml-neural - network.
;;   
;;
;; Copyright (c) 2019 Simon Johnston (simonjo@amazon.com).

;; Racket Style Guide: http://docs.racket-lang.org/style/index.html

(require racket/contract)

(provide
 (contract-out))

;; ---------- Requirements

(require racket/bool
         racket/flonum
         racket/vector
         (except-in math/flonum
                    fltanh)
         "activation.rkt")

;; ---------- Internal types

;; ---------- Implementation (neuron)

(struct neuron
  (type ; (or/c 'input 'hidden 'output)
   bias ; flonum
   inputs ; hash
   [emitted #:mutable]) #:transparent)

(define (input-neuron v)
  (neuron 'input #f #f (value-or-random v)))

(define (hidden-neuron b i)
  (neuron 'hidden (value-or-random b) (if (hash? i) i (make-hasheq)) #f))

(define (output-neuron b i)
  (neuron 'output (value-or-random b) (if (hash? i) i (make-hasheq)) #f))

(define (fire-neuron n activation-function)
  (unless
      (equal? (neuron-type n) 'input)
    (define result
      (+ 
       (flvector-sum
         (for/flvector ([(from weight) (neuron-inputs n)])
           (fl* (neuron-emitted from) weight)))
       (neuron-bias n)))
    (define w (vector (hash-values (neuron-inputs n))))
    (define x (vector (map neuron-emitted (hash-keys (neuron-inputs n)))))
    (displayln (format "(w=~a ∙ x=~a) + b=~a = ~a" w x (neuron-bias n) result))
    (set-neuron-emitted! n (activation-function result))))

;; ---------- Implementation (connections)

(struct input
  (from
   weight))

(define (create-connection layer-from from-index layer-to to-index weight)
  ;; do not currently support https://en.wikipedia.org/wiki/Recurrent_neural_network
  (when (equal? layer-from layer-to)
    (error "may not add connections within a layer"))
  (when (equal? (layer-type layer-to) 'input)
    (error "may not add connections into an input layer"))
  (when (equal? (layer-type layer-from) 'output)
    (error "may not add connections from an output layer"))
  (define to-neuron (vector-ref (layer-neurons layer-to) to-index))
  (define from-neuron (vector-ref (layer-neurons layer-from) from-index))
  (cond
    [(hash-has-key? (neuron-inputs to-neuron) from-neuron)
     (hash-set! (neuron-inputs to-neuron)
                from-neuron
                (+ (hash-ref (neuron-inputs to-neuron) from-neuron) (value-or-random weight)))]
    [else
     (hash-set! (neuron-inputs to-neuron) from-neuron (value-or-random weight))]))  

(define (connect-layers/fully layer-from layer-to weight)
  (for* ([i (in-range (vector-length (layer-neurons layer-from)))]
         [j (in-range (vector-length (layer-neurons layer-to)))])
    (define real-weight (vector-or-value-or-random weight i))
    (create-connection layer-from i layer-to j real-weight)))

;; ---------- Implementation (layer)

(struct layer
  (type
   neurons) #:transparent)

(define (create-layer type number-of-neurons constructor)
  (layer type (build-vector number-of-neurons constructor)))

(define (create-input-layer number-of-neurons [input-value #f])
  (create-layer 'input number-of-neurons
                (λ (i) (input-neuron (vector-or-value-or-random input-value i)))))

(define (create-hidden-layer number-of-neurons [bias-value #f])
  (create-layer 'hidden number-of-neurons
                (λ (i) (hidden-neuron (vector-or-value-or-random bias-value i) #f))))

(define (create-output-layer number-of-neurons [bias-value #f])
  (create-layer 'output number-of-neurons
                (λ (i) (output-neuron (vector-or-value-or-random bias-value i) #f))))

(define (layer-forward l activation-function)
  (unless
      (equal? (layer-type l) 'input)
    (for ([n (layer-neurons l)])
      (fire-neuron n activation-function))))

;; ---------- Implementation (network)

(struct network
  (layers
   forward-function
   backward-function))

(define (create-network layers activation-function)
  (when (< (vector-length layers) 2)
    (error "need more than one layer dufus"))
  (when (not (equal? (layer-type (vector-ref layers 0)) 'input))
    (error "your first layer needs to be an input layer"))
  (when (not (equal? (layer-type (vector-ref layers (- (vector-length layers) 1))) 'output))
    (error "your last layer needs to be an output layer"))
  (network layers
           (activator-f activation-function)
           (activator-df activation-function)))

(define (create-network/fully-connected layer-sizes activation-function)
  (define layer-vector (make-vector (vector-length layer-sizes) #f))  
  (vector-set! layer-vector 0 (create-input-layer (vector-ref layer-sizes 0) 0.0))
  (for ([i (in-range 1 (vector-length layer-sizes))])
    (vector-set! layer-vector i (create-hidden-layer (vector-ref layer-sizes i))))
  (define last-layer (- (vector-length layer-sizes) 1))
  (vector-set! layer-vector last-layer (create-output-layer (vector-ref layer-sizes last-layer) 0.0))
  (for ([i (in-range 1 (vector-length layer-sizes))])
    (connect-layers/fully (vector-ref layer-vector (- i 1)) (vector-ref layer-vector i) #f))
  (create-network layer-vector activation-function))
  
(define (network-forward net)
  (define layers (network-layers net))
  (for ([layer (in-range 1 (vector-length layers))])
    (layer-forward (vector-ref layers layer) (network-forward-function net)))
  (define last-layer (layer-neurons (vector-ref layers (- (vector-length layers) 1))))
  (for/flvector ([neuron last-layer])
    (neuron-emitted neuron)))

(define (reset-input net in-vector)
  (define input-layer (vector-ref (network-layers net) 0))
  (when (not (equal? (vector-length (layer-neurons input-layer)) (vector-length in-vector)))
    (error "input value vector does not match number of input neurons"))
  (define neurons (layer-neurons (vector-ref (network-layers net) 0)))
  (for ([i (in-range (vector-length in-vector))])
    (define neuron (vector-ref neurons i))
    (set-neuron-emitted! neuron (vector-ref in-vector i))))

;; ---------- Internal procedures

(define (value-or-random v)
  (if (false? v)
      (random)
      v))

(define (vector-or-value-or-random v [i #f])
  (cond
    [(flonum? v)   v]
    [(flvector? v) (flvector-ref v i)]
    [(false? v)    (random)]
    [else (error "invalid type for value: " v)]))

(define (dot-product v1 v2)
  (flvector-sum
   (flvector* v1 v2)))
             
(define ∙ dot-product)

;; ---------- Internal tests

(define nand-in (create-input-layer 2 (flvector 0.0 0.0)))
(define nand-out (create-output-layer 1 (flvector 3.0)))
(connect-layers/fully nand-in  nand-out -2.0)
(define nand-gate (create-network (vector nand-in nand-out) flbinary-perceptron))

(displayln (network-forward nand-gate))
(reset-input nand-gate (vector 1.0 0.0))
(displayln (network-forward nand-gate))
(reset-input nand-gate (vector 0.0 1.0))
(displayln (network-forward nand-gate))
(reset-input nand-gate (vector 1.0 1.0))
(displayln (network-forward nand-gate))
              
(define image-in (create-input-layer (* 28 28) 0.0))
(define image-hidden (create-hidden-layer 15 0.0))
(connect-layers/fully image-in image-hidden #f)
(define image-out (create-output-layer 10 0.0))
(connect-layers/fully image-hidden image-out #f)
(define image-to-number (create-network (vector image-in image-hidden image-out) flbinary-perceptron))

(create-network/fully-connected (vector (* 28 28) 15 10) flsigmoid)

