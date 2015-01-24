(define (square-tree tree)
  (define nil '())
  (cond ((null? tree) nil)
	((pair? tree) (cons (square-tree (car tree))
			    (square-tree (cdr tree))))
	(else (square tree))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square x) (* x x))

(define (map-square tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (map-square subtree)
	     (square subtree)))
       tree))

(map-square (list 1
		  (list 2 (list 3 4) 5)
		  (list 6 7)))
