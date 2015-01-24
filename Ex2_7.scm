(define (make-interval a b) (cons a b))
(define (upper-bound interval) (max (car interval) (cdr interval)))
(define (lower-bound interval) (min (car interval) (cdr interval)))

(upper-bound (make-interval 2 3))
(lower-bound (make-interval 3 2))
