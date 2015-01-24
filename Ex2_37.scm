(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init sequence)
  (define nil '())
  (if (null? (car sequence))
      nil
      (cons (accumulate op init (map car sequence))
	    (accumulate-n op init (map cdr sequence)))))

(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))

(define (dot-product v1 v2)
  (accumulate + 0 (map * v1 v2)))

(dot-product (list 1 2 3) (list 4 5 6))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v))
       m))

(matrix-*-vector matrix (list 2 3 4 5))

(define nil '())

(define (transpose m)
  (accumulate-n cons nil m))

(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row)
	   (map (lambda (n-col)
		  (dot-product m-row n-col))
		n-cols))
	 m)))

(matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2)))
