(define empty-board '())
(define nil '())

(define (place-queen rank file)
  (cons rank file))

(define (queen-rank queen)
  (car queen))

(define (queen-file queen)
  (cdr queen))

(define (adjoin-position rank file board)
  (cons (place-queen rank file)
	board))

(define (find-first pred items)
  (cond ((null? items) nil)
	((pred (car items))
	 (car items))
	(else (find-first pred (cdr items)))))

(define (safe? file board)
  (define (get-queen-by-file file board)
    (find-first (lambda (queen)
		  (= (queen-file queen) file))
		board))

  (let* ((the-queen
	  (get-queen-by-file file board))
	 (other-queens
	  (filter (lambda (q)
		    (not (and (= (queen-rank the-queen)
				 (queen-rank q))
			      (= (queen-file the-queen)
				 (queen-file q)))))
		  board)))
    (and (not (accumulate (lambda (p q)
			    (or q
				(= (queen-rank q)
				   (queen-rank the-queen))))
			  #f
			  other-queens))
	 (not (accumulate (lambda (p q)
			    (or q
				(= (abs (- (queen-rank the-queen)
					   (queen-rank p)))
				   (abs (- (queen-file the-queen)
					   (queen-file p))))))
			  #f
			  other-queens)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (position) (safe? k position))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(queens 4)
