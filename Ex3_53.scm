(define stream-null? null?)

(define the-empty-stream '())

(define stream-car
  (lambda (s)
    (car (force s))))

(define stream-cdr
  (lambda (s)
    (cdr (force s))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (delay (cons
	      (apply proc (map stream-car argstreams))
	      (apply stream-map
		     (cons proc (map stream-cdr argstreams)))))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (delay (cons
	      low
	      (stream-enumerate-interval (+ low 1) high)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car))
	 (delay (cons (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream)))))
	(else (stream-filter pred (stream-cdr stream)))))

(define (add-stream s1 s2)
  (stream-map + s1 s2))
;;; 2的n次幂				
(define s (delay (cons 1 (add-stream s s))))

(stream-ref s 4)

;; Ex3_54
(define (integer-starting-from n)
  (delay (cons n (integer-starting-from (+ n 1)))))

(define integers (integer-starting-from 1))

(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define factorials (delay (cons 1 (mul-stream factorials (stream-cdr integers)))))

(stream-ref factorials 3)

;; Ex3_55

(define (partial-sums s)
  (delay
    (cons (stream-car s)
	  (add-stream (stream-cdr s) (partial-sums s)))))

(stream-ref (partial-sums integers) 3)

;; Ex3_56

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (delay (cons s1car
			       (merge (stream-cdr s1) s2))))
		 ((> s1car s2car)
		  (delay (cons s2car
			       (merge s1 (stream-cdr s2)))))
		 (else
		  (delay (cons s1car
			       (merge (stream-cdr s1)
				      (stream-cdr s2))))))))))

(define S (delay (cons 1 (merge (merge (scale-stream S 2) (scale-stream S 3))
				(scale-stream S 5)))))

;; Ex3_59
(define ones (delay (cons 1 ones)))

(define (integrate-series s)
  (stream-map * (stream-map / ones integers) s))

(define sine-series (delay
		      (cons 0 (integrate-series cosine-series))))

(define cosine-series (delay
			(cons 1 (integrate-series (scale-stream
						   sine-series
						   -1)))))
