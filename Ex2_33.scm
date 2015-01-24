(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (mop p sequence)
  (accumulate (lambda (x y)
		(cons (p x) y))
	      nil
	      sequence))

(map (lambda (x) (* x x)) (list 1 1 2))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(my-append (list 2 4) (list 3 5))

(define (my-length sequence)
  (accumulate (lambda (x y)
		(+ 1 y))
	      0 sequence))

(my-length (list 2 2 34))
