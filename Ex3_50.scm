(define stream-null? null?)

(define the-empty-stream '())

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? #t)
		 result)
	  result))))

(define (delay exp)
  (memo-proc (lambda () exp)))

(define (force delayed-exp)
  (delayed-exp))

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

(define stream-arg (cons-stream 1 (cons-stream 2 (cons-stream 3 '()))))

(stream-map + stream-arg stream-arg stream-arg)


