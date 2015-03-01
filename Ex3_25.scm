(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
	    ((same-key? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))
    (define (lookup key-list)
      (define (iter-lookup l table)
	(cond ((null? l) #f)
	      ((= 1 (length l))
	       (let ((record (assoc (car l) (cdr table))))
		 (if record (cdr record) #f)))
	      (else
	       (let ((subtable (assoc (car l) (cdr table))))
		 (if subtable
		     (iter-lookup (cdr l) subtable)
		     #f)))))
      (iter-lookup key-list local-table))
    (define (insert! key-list value)
      (define (iter-insert! l table)
	(cond ((null? l) (error "Empty list" l))
	      ((= 1 (length l))
	       (let ((record (assoc (car l) (cdr table))))
		 (if record
		     (set-cdr! record value)
		     (set-cdr! table
			       (cons (cons (car l) value)
				     (cdr table))))))
	      (else
	       (let ((subtable (assoc (car l) (cdr table))))
		 (if subtable
		     (iter-insert! (cdr l) subtable)
		     (set-cdr! table
			       (cons (list (car l)
					   (iter-insert! (cdr l) subtable))
				     (cdr table))))))))
      (iter-insert! key-list local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

