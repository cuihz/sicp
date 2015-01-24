(define x (list (list 1 2) (list 3 4)))

(define (fringe tree)
  (define nil '())
  (if (null? tree)
      nil
      (let ((first (car tree)))
	(if (not (pair? first))
	    (cons first (fringe (cdr tree)))
	    (append (fringe first) (fringe (cdr tree)))))))

(fringe (list x x))

