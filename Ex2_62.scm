(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2))
		    (rest1 (cdr set1))
		    (rest2 (cdr set2)))
		(cond ((= x1 x2)
		       (cons x1
			     (union-set rest1 rest2)))
		      ((< x1 x2)
		       (cons x1
			     (union-set rest1 set2)))
		      ((< x2 x1)
		       (cons x2
			     (union-set rest2 set1))))))))

(union-set (list 1 6 9) (list 2 7 8))
