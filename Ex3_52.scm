(define stream-null? null?)

(define the-empty-stream '())

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (begin (display (stream-cdr s))
	     (stream-ref (stream-cdr s) (- n 1)))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))


(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))


(stream-ref (stream-enumerate-interval 1 20) 5)

(define (integer-starting-from n)
  (cons-stream n (integer-starting-from (+ n 1))))

(define integers (integer-starting-from 1))

(stream-ref integers 10)

