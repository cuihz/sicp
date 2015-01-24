(define (square-list items)
  (define (square x) (* x x))
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

(square-list-2 (list 1 2 3 4))
