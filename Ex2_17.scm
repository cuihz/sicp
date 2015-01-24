(define (last-pair list)
  (if (null? list)
      (error "Empty list as parameter" list)
      (let ((rest (cdr list)))
	(if (null? rest)
	    (car list)
	    (last-pair rest)))))

(last-pair (list 3 4 5 56))
