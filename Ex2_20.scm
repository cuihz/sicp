(define (same-parity x . y)
  (define (iter list result)
    (if (null? list)
	result
	(let ((first-element (car list))
	      (same-x? (if (even? x) even? odd?)))
	  (if (same-x? first-element)
	      (iter (cdr list) (cons first-element result))
	      (iter (cdr list) result)))))
  (iter y '()))

(same-parity 1 2 3 4 5 6 7)
