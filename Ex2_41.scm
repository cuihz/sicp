(define nil '())

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

(define (sum-3 n s)
  (filter (lambda (list)
	    (= s (accumulate + 0 list)))
	  (differ-3 n)))

(define (differ-3 n)
  (flatmap (lambda (x)
	     (flatmap (lambda (y)
			(map (lambda (z)
			       (list x y z))
			     (enumerate-interval (+ y 1) n)))
		      (enumerate-interval (+ x 1) n)))
	   (enumerate-interval 1 n)))

(sum-3 6 10)
