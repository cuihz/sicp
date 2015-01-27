(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((< x (car set)) #f)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (let ((first (car set)))
    (cond ((= x first) set)
	  ((< x first)
	   (cons x set))
	  (else (cons first (adjoin-set x (cdr set)))))))

(adjoin-set 5 (list 1 3 6 7))
