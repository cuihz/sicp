(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (make-sum-list (list a1 a2)))))

(define (make-sum-list l)
  (if (= (length l) 2)
      (list '+ (car l) (cadr l))
      (make-sum (car l) (make-sum-list (cdr l)))))

(define (make-product-list l)
  (if (= (length l) 2)
      (list '* (car l) (cadr l))
      (make-product (car l) (make-product-list (cdr l)))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (make-product-list (list m1 m2)))))

(append (list 1 23) (list 'a))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (let ((a (cddr s)))
    (if (= (length a) 1)
	(car a)
	(make-sum-list a))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (let ((m (cddr p)))
    (if (= (length m) 1)
	(car m)
	(make-product-list m))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) 'expt)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation b e)
  (cond ((= e 0) 1)
	((= e 1) b)
	(else (list 'expt b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum

	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (let ((n (exponent exp))
	       (u (base exp)))
	   (make-product
	    (make-product n (make-exponentiation u (- n 1)))
	    (deriv u var))))
	(else
	 (error "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(expt x 3) 'x)

(deriv '(+ x y z) 'x)

(deriv '(* x y (+ x 3)) 'x)

(deriv '(+ (* x 3) (* x y) (* x 4)) 'x)

(make-sum-list '(y z))
(length '(y z))
(list '+ (car '(y z)) (cadr '(y z)))
