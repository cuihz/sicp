(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (pair? s)
	(total-weight s)
	s)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (if-balance mobile)
  (define (torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (let ((left (left-branch mobile))
	(right (right-branch mobile))
	(s-left (branch-structure (left-branch mobile)))
	(s-right (branch-structure (right-branch mobile))))
    (if (= (torque left) (torque right))
	(cond ((and (pair? s-left) (pair? s-right))
	       (and (if-balance s-left) (if-balance s-right)))
	      ((pair? s-left) (if-balance s-left))
	      ((pair? s-right) (if-balance s-right))
	      (else #t))
	#f)))

(define level-1-mobile (make-mobile (make-branch 2 1)
				    (make-branch 1 2)))

(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile)
				    (make-branch 9 1)))

(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile)
				    (make-branch 8 2)))

(if-balance level-3-mobile)

