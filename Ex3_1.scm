(define (make-accumulator sum)
  (lambda (count)
    (begin (set! sum (+ sum count))
	   sum)))

(define A (make-accumulator 5))

(A 10)
(A 10)
