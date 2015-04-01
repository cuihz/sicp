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

(define stream-arg (delay (cons 1 (delay (cons 2 (delay (cons 3 '())))))))

(stream-map + stream-arg stream-arg stream-arg)

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

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

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (delay 
	(cons (proc (stream-car s))
	      (stream-map proc (stream-cdr s))))))

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)

(stream-ref x 7)

;;; Ex3_52
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define counters
  (let next ((n 1))
    (delay (cons n (next (+ n 1))))))

(define x (stream-enumerate-interval 1 20))

(define (integer-starting-from n)
  (delay (cons n (integer-starting-from (+ n 1)))))

(define integers (integer-starting-from 1))

(stream-ref integers 10)

(stream-car integers)
(car (force integers))
