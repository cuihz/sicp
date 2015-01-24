(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (count-times n divisor)
  (define (iter try-exp)
    (if (= 0 (remainder n (expt divisor try-exp)))
	(iter (+ try-exp 1))
	(- try-exp 1)))
  (iter 1))

(define (car x)
  (count-times x 2))

(define (cdr x)
  (count-times x 3))

(cdr 12)

