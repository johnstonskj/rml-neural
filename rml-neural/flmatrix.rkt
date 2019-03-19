#lang racket/base

(require racket/flonum
         racket/vector
         math/flonum)

;; row-major
(define (flmatrix? v)
  (and (vector? v)
       (for/and ([row v]) (flvector? row))))

(define (make-flmatrix rows columns [value 0.0])
  (when (or (= rows 0) (= columns 0))
    (error "cannot create a matrix with 0 dimension"))
  (for/vector ([row (in-range rows)])
    (make-flvector columns value)))

(define (build-flmatrix rows columns proc)
  (when (or (= rows 0) (= columns 0))
    (error "cannot create a matrix with 0 dimension"))
  (for/vector ([row (in-range rows)])
    (build-flvector columns (λ (col) (proc row col)))))

(define (flmatrix-shape m)
  (values (vector-length m) (flvector-length (vector-ref m 0))))

(define (flmatrix-row-copy m row)
  (flvector-copy (vector-ref m row)))

(define (flmatrix-column-copy m column)
  (for/flvector ([row m])
    (flvector-ref (vector-ref m row) column)))

(define (flmatrix->flvector/row-wise m)
  (define-values (rows columns) (flmatrix-shape m))
  (for*/flvector ([row (in-range rows)]
                  [column (in-range columns)])
    (flvector-ref (vector-ref m row) column)))
                 
(define (flmatrix->flvector/column-wise m)
  (define-values (rows columns) (flmatrix-shape m))
  (for*/flvector ([column (in-range columns)]
                  [row (in-range rows)])
    (flvector-ref (vector-ref m row) column)))
                 
(define (flmatrix-ref m row column)
  (flvector-ref (vector-ref m row) column))

(define (flmatrix-set! m row column v)
  (flvector-set! (vector-ref m row) column v))

(module+ test
  (require rackunit)
  
  (define m (make-flmatrix 1 4))
  (define-values (r c) (flmatrix-shape m))
  (check-eq? r 1)
  (check-eq? c 4)

  (check-equal? (flmatrix-ref m 0 0) 0.0)
  (flmatrix-set! m 0 0 22.0)
  (check-equal? (flmatrix-ref m 0 0) 22.0)

  (define m2 (build-flmatrix 4 4 (λ (r c) (fl (+ r c)))))
  (flmatrix-set! m2 0 0 (fl 11))
  (flmatrix-set! m2 1 0 (fl 22))
  (flmatrix-set! m2 2 0 (fl 33))
  (flmatrix-set! m2 3 0 (fl 44))

  (check-equal? m2
                (vector (flvector 11.0 1.0 2.0 3.0)
                        (flvector 22.0 2.0 3.0 4.0)
                        (flvector 33.0 3.0 4.0 5.0)
                        (flvector 44.0 4.0 5.0 6.0)))

  (check-equal? (flmatrix->flvector/row-wise m2)
                (flvector 11.0 1.0 2.0 3.0 22.0 2.0 3.0 4.0 33.0 3.0 4.0 5.0 44.0 4.0 5.0 6.0))
  
  (check-equal? (flmatrix->flvector/column-wise m2)
                (flvector 11.0 22.0 33.0 44.0 1.0 2.0 3.0 4.0 2.0 3.0 4.0 5.0 3.0 4.0 5.0 6.0)))
