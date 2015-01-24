(define (for-each func list)
  (let ((cdr-list (cdr list)))
    (func (car list))
    (if (not (null? cdr-list))
	(for-each func cdr-list)
	#t)))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))
