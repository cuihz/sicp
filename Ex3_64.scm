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

(define (display-stream s n)
  (if (= n 0)
      (newline)
      (begin (display (stream-car s))
	     (newline)
	     (display-stream (stream-cdr s) (- n 1)))))

;; Ex3_64
(define average
  (lambda (x y) (/ (+ x y) 2)))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (delay (cons 1.0
		 (stream-map (lambda (guess)
			       (sqrt-improve guess x))
			     guesses))))
  guesses)

(define stream-limit
  (lambda (s tolerance)
    (let ((s1 (stream-car s))
	  (s2 (stream-car (stream-cdr s))))
      (if (< (abs (- s1 s2)) tolerance)
	  s2
	  (stream-limit (stream-cdr s) tolerance)))))

(define (my-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(my-sqrt 9.0 0.001)

;; Ex3_65
(define (ln-summands n)
  (delay (cons (/ 1.0 n)
	       (stream-map - (ln-summands (+ n 1))))))

(define ln-stream
  (let ((ln-summands-1 (ln-summands 1)))
    (delay (cons (stream-car ln-summands-1)
		 (stream-map + ln-stream (stream-cdr ln-summands-1))))))

(define (euler-transform s)
  (define (square x)
    (* x x))
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (delay (cons (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s))))))

(define (make-tableau transform s)
  (delay (cons s
	       (make-tableau transform
			     (transform s)))))

(define (accelerate-squence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

(define ln-result (accelerate-squence euler-transform
				      ln-stream))

(display-stream ln-result 8)

