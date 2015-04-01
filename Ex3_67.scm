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
	((pred (stream-car stream))
	 (delay (cons (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream)))))
	(else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s n)
  (if (= n 0)
      (newline)
      (begin (display (stream-car s))
	     (newline)
	     (display-stream (stream-cdr s) (- n 1)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (delay (cons (stream-car s1)
		   (interleave s2 (stream-cdr s1))))))

(define (all-pairs s t)
  (delay (cons
	  (list (stream-car s) (stream-car t))
	  (interleave
	   (interleave
	    (stream-map (lambda (x) (list (stream-car s) x))
			(stream-cdr t))
	    (all-pairs (stream-cdr s) (stream-cdr t)))
	   (stream-map (lambda (x) (list x (stream-car t)))
		       (stream-cdr s))))))

(define (integer-starting-from n)
  (delay (cons n (integer-starting-from (+ n 1)))))

(define integers
  (integer-starting-from 1))

(display-stream (all-pairs integers integers) 8)

;; Ex3_69

(define (pairs s t)
  (delay (cons (list (stream-car s)
		     (stream-car t))
	       (interleave
		(stream-map (lambda (x) (list (stream-car s) x))
			    (stream-cdr t))
		(pairs (stream-cdr s) (stream-cdr t))))))

(define (triples s t u)
  (delay (cons (list (stream-car s)
		     (stream-car t)
		     (stream-car u))
	       (interleave
		(stream-map (lambda (x)
			      (cons (stream-car s) x))
			    (stream-cdr (pairs t u)))
		(triples (stream-cdr s)
			 (stream-cdr t)
			 (stream-cdr u))))))

(define (square x) (* x x))

(define numbers (triples integers integers integers))

(define phythagorean-numbers
  (stream-filter
   (lambda (x)
     (= (square (caddr x))
	(+ (square (car x)) (square (cadr x)))))
   numbers))

(display-stream phythagorean-numbers 5)






