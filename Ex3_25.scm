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
		     (let ((sub-table (list (car l))))
		       (begin (iter-insert! (cdr l) sub-table)
			      (set-cdr! table
					(cons sub-table
					      (cdr table))))))))))
         (if (null? (cdr table)) 
             (begin (display "-no entries-") 
                    (newline)) 
             (for-each (lambda (record) 
                         (print-record record level)) 
                       (cdr table)))) 
  
       (print-table local-table 0))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    ((eq? m 'print) print)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define op-table (make-table eq?))

(define put (op-table 'insert-proc!))

(define get (op-table 'lookup-proc))

((op-table 'print))

(put '(letter a) 97)

(put '(letter b ) 98)

(put '(math +) 43)

(put '(math -) 45)

(put '(math *) 42)

(put '(greek majiscule Λ) 923)

(put '(greek miniscule λ) 955)

(put '(min) 42)

(put '(max) 955)

(get '(min))

(get '(letter b))

(get '(greek miniscule λ))

(get '(dfs))

((op-table 'print))
