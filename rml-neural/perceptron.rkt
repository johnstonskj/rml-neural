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
  (neuron 'input #f #f (get-value v)))

(define (hidden-neuron b i)
  (neuron 'hidden (get-value b) (if (hash? i) i (make-hasheq)) #f))

(define (output-neuron b i)
  (neuron 'output (get-value b) (if (hash? i) i (make-hasheq)) #f))

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
                (+ (hash-ref (neuron-inputs to-neuron) from-neuron) (get-value weight)))]
    [else
     (hash-set! (neuron-inputs to-neuron) from-neuron (get-value weight))]))  

(define (connect-layers/one-to-one layer-from  layer-to weight)
  (when (not (equal? (vector-length (layer-neurons layer-from))
                     (vector-length (layer-neurons layer-to))))
    (error "requires layers of equal length"))
  (when (and (flvector? weight)
             (not (equal? (vector-length (layer-neurons layer-from))
                          (vector-length weight))))
    (error "weight vector length must match layer length"))
  (for ([i (in-range (layer-neurons layer-from))])
    (define real-weight
      (cond
      [(flonum? weight)   weight]
      [(flvector? weight) (vector-ref weight i)]
      [(false? weight)    (random)]
      [else (error "invalid type for weight")]))
    (create-connection layer-from i layer-to i real-weight)))

(define (connect-layers/fully layer-from layer-to weight)
  (for* ([i (in-range (vector-length (layer-neurons layer-from)))]
         [j (in-range (vector-length (layer-neurons layer-to)))])
    (define real-weight
      (cond
      [(flonum? weight)   weight]
      [(flvector? weight) (vector-ref weight i)]
      [(false? weight)    (random)]
      [else (error "invalid type for weight")]))
    (create-connection layer-from i layer-to j real-weight)))

;; ---------- Implementation (layer)

(struct layer
  (type
   neurons) #:transparent)

(define (create-layer type number-of-neurons constructor)
  (layer type (build-vector number-of-neurons constructor)))

(define (create-input-layer number-of-neurons [input-vector #f])
  (define values
    (if (false? input-vector)
        (make-vector number-of-neurons #f)
        input-vector))
  (create-layer 'input number-of-neurons (λ (i) (input-neuron (vector-ref values i)))))

(define (create-hidden-layer number-of-neurons [bias-vector #f])
  (define biases
    (if (false? bias-vector)
        (make-vector number-of-neurons #f)
        bias-vector))
  (create-layer 'hidden number-of-neurons (λ (i) (hidden-neuron (vector-ref biases i) #f))))

(define (create-output-layer number-of-neurons [bias-vector #f])
  (define biases
    (if (false? bias-vector)
        (make-vector number-of-neurons #f)
        bias-vector))
  (create-layer 'output number-of-neurons (λ (i) (output-neuron (vector-ref biases i) #f))))

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

(define (get-value f)
  (if (false? f)
      (random)
      f))

(define (dot-product v1 v2)
  (flvector-sum
   (flvector* v1 v2)))
             
(define ∙ dot-product)

;; ---------- Internal tests

(define nand-in (create-input-layer 2 (vector 0.0 0.0)))
(define nand-out (create-output-layer 1 (vector 3.0)))
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
(define image-hidden (create-input-layer 15 0.0))
(connect-layers/fully image-in image-hidden #f)
(define image-out (create-output-layer 10 0.0))
(connect-layers/fully image-hidden image-out #f)
(define image-to-number (create-network (vector image-in image-hidden image-out) flbinary-perceptron))

