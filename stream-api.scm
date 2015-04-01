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

;; delay and force defined as below
;; (define-syntax delay
;;   (syntax-rules ()
;;     ((_ exp) (make-promise (lambda () exp)))))

;; (define make-promise
;;   (lambda (p)
;;     (let ((val #f) (set? #f))
;;       (lambda ()
;; 	(if (not set?)
;; 	    (let ((x (p)))
;; 	      (if (not set?)
;; 		  (begin (set! val x)
;; 			 (set! set? #f)))))
;; 	val))))

;; (define force
;;   (lambda (promise)
;;     (promise)))


