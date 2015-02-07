(define f
  (let ((init (- 1)))
    (lambda (x) (if (= x 0)
		    (begin (set! init 0)
			   init)
		    (if (= init 0)
			init
			x)))))

(f 1)
(f 0)
(f 1)
