(define (subsets s)
  (define nil '())
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (subsets-element)
			    (cons (car s) subsets-element))
			  rest)))))
 
(subsets (list 1 2 3))

(cons 3 (list 3 5))
