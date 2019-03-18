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
         math/flonum)

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
  (neuron 'hidden (get-value b) (if (hash? i) i (make-hash)) #f))

(define (output-neuron b i)
  (neuron 'output (get-value b) (if (hash? i) i (make-hash)) #f))

(define (emit n)
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
    (set-neuron-emitted! n (cond
                             [(fl<= result 0.0) 0.0]
                             [else              1.0]))))

;; ---------- Implementation (connections)

(struct input
  (from
   weight))

(define (create-connection layer-from from-index layer-to to-index weight)
  (when (equal? layer-from layer-to)
    (error "may not add connections within a layer"))
  (when (equal? (layer-type layer-to) 'input)
    (error "may not add connections into an input layer"))
  (when (equal? (layer-type layer-from) 'output)
    (error "may not add connections from an output layer"))
  (define to-neuron (vector-ref (layer-neurons layer-to) to-index))
  ;; make hash of input-neuron -> weight
  (define from-neuron (vector-ref (layer-neurons layer-from) from-index))
  (cond
    [(hash-has-key? (neuron-inputs to-neuron) from-neuron)
     (hash-set! (neuron-inputs to-neuron) from-neuron (+ (hash-ref (neuron-inputs to-neuron) from-neuron) (get-value weight)))]
    [else
     (hash-set! (neuron-inputs to-neuron) from-neuron (get-value weight))]))  

(define (connect-layers/one-to-one layer-from  layer-to weight)
  (when (not (equal? (vector-length (layer-neurons layer-from)) (vector-length (layer-neurons layer-to))))
    (error "requires layers of equal length"))
  (when (and (flvector? weight) (not (equal? (vector-length (layer-neurons layer-from)) (vector-length weight))))
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

(define (layer-forward l)
  (unless
      (equal? (layer-type l) 'input)
    (for ([n (layer-neurons l)])
      (emit n))))

;; ---------- Implementation (network)

(struct network
  (layers))

(define (create-network layers)
  (when (< (vector-length layers) 2)
    (error "need more than one layer dufus"))
  (when (not (equal? (layer-type (vector-ref layers 0)) 'input))
    (error "your first layer needs to be an input layer"))
  (when (not (equal? (layer-type (vector-ref layers (- (vector-length layers) 1))) 'output))
    (error "your last layer needs to be an output layer"))
  (network layers))

(define (network-forward net)
  (define layers (network-layers net))
  (for ([layer (in-range 1 (vector-length layers))])
    (layer-forward (vector-ref layers layer)))
  (define last-layer (layer-neurons (vector-ref layers (- (vector-length layers) 1))))
  (for/flvector ([neuron last-layer])
    (neuron-emitted neuron)))

(define (reset-input net in-vector)
  (define input-layer (vector-ref (network-layers net) 0))
  (when (not (equal? (vector-length input-layer) (vector-length in-vector)))
    (error "input value vector does not match number of input neurons"))
  (for (
  )

;; ---------- Internal procedures

(define (get-value f)
  (if (false? f)
      (random)
      f))

(define (dot-product v1 v2)
  (flvector-sum
   (flvector-map fl* v1 v2)))
             
(define ∙ dot-product)

;; ---------- Internal tests

(define nand-in (create-input-layer 2 (vector 1.0 0.0)))
(define nand-out (create-output-layer 1 (vector 3.0)))
(connect-layers/fully nand-in  nand-out -2.0)
(define nand-gate (create-network (vector nand-in nand-out)))
(displayln (network-forward nand-gate))

(define input-layer (create-input-layer 4))
(define output-layer (create-output-layer 6))
(connect-layers/fully input-layer  output-layer 3.0)

(define net (create-network (vector input-layer output-layer)))

(displayln (network-forward net))
