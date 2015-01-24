(define nil '())

(define (reverse items)
  (define (iter item result)
    (if (null? item)
	result
	(iter (cdr item) (cons (car item) result))))
  (iter items nil))

(define (deep-reverse items)
  (define (deep-rev-imp items result)
    (if (null? items)
	result
	(let ((first (car items)))
	  (deep-rev-imp (cdr items)
		(cons (if (not (pair? first))
			  first
			  (deep-reverse first))
		      result)))))
  (deep-rev-imp items nil))

(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)
