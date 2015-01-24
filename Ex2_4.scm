(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons 3 4))

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 3 4))
