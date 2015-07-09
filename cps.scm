(define retry #f)

(define factorial
  (lambda (x)
    (if (= x 0)
	(call/cc
	 (lambda (k)
	   (set! retry k)
	   1))
	(* x (factorial (- x 1))))))

(define (fact-cps n k)
  (if (= n 0)
      (k 1)
      (fact-cps (- n 1)
		(lambda (v)
		  (k (* n v))))))

(define (k n) n)

(fact-cps 3 k)

(fact-cps 2
	  (lambda (v)
	    (k (* 3 v))))

(fact-cps 1
	  (lambda (v)
	    ((lambda (v)
	       (k (* 3 v))) (* 2 v))))

(fact-cps 0
	  (lambda (v)
	    ((lambda (v)
	       ((lambda (v)
		  (k (* 3 v))) (* 2 v))) (* 1 v))))

((lambda (v)
   ((lambda (v)
      ((lambda (v)
	 (k (* 3 v))) (* 2 v))) (* 1 v))) 1)

((lambda (v)
   ((lambda (v)
      (k (* 3 v))) (* 2 v))) (* 1 1))

((lambda (v)
   (k (* 3 v))) (* 2 1))

(k (* 3 2))

(k 6)

6
