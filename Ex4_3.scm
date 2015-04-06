;; 定义数据导向型的二维表格
(define (make-table)
  (define (assoc key records)
  (cond ((null? records) #f)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; 定义表格和表格操作
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; 填充表格
(put 'op 'quote text-of-quotation)
(put 'op 'set! eval-assignment)
(put 'op 'define eval-definition)
(put 'op 'if eval-if)
(put 'op 'lambda (lambda (x y) (make-procedure (lambda-parameters x)
					       (lambda-body x)
					       y)))
(put 'op 'begin (lambda (x y) (eval-sequence (begin-sequence x) y)))
(put 'op 'cond (lambda (x y) (evaln (cond->if x) y)))

;; eval内核
(define (evaln expr env)
  (cond ((self-evaluating? expr) expr)
	((variables? expr) (lookup-variable-value expr env))
	((get 'op (car expr)) (applyn (get 'op (car expr) expr env)))
	((application? expr) (applyn (evaln (operator expr) env)
				     (list-of-values (operands expr) env)))
	(else
	 (error "Unkown expression type -- EVALN" expr))))
