(define nil '())

(define (reverse items)
  (define (iter item result)
    (if (null? item)
	result
	(iter (cdr item) (cons (car item) result))))
  (iter items nil))

(reverse (list 3 4 56 6))


