(define (make-person firstName lastName)
  (let ((person (list 'person firstName lastName)))
    (define (get-firstName)
      (cadr person))
    (define (get-lastName)
      (caddr person))
    (define (print)
      (begin (display firstName)
	     (display '--)
	     (display lastName)
	     (newline)))
    (define (dispatch m)
      (cond ((eq? m 'firstName) (get-firstName))
	    ((eq? m 'lastName) (get-lastName))
	    ((eq? m 'print) (print))))
    dispatch))

(define (ordering less-than-func exact-func)
  (define (ad-join x set)
    (cond ((null? set) (list x))
	  ((less-than-func (exact-func x)
			   (exact-func (car set))) (cons x set))
	  (else (cons (car set)
		      (ad-join x (cdr set))))))
  (define (iter unordered-list)
    (cond ((null? unordered-list) '())
	  ((null? (cdr unordered-list)) (ad-join (car unordered-list) '()))
	  (else (ad-join (car unordered-list)
			 (iter (cdr unordered-list))))))
  (lambda (un-list)
    (iter un-list)))

(define (reverse items)
  (define (iter item result)
    (if (null? item)
	result
	(iter (cdr item) (cons (car item) result))))
  (iter items '()))

(define (exact-person-firstName person)
  (person 'firstName))

(define person1 (make-person "hanze" "cui"))
(define person2 (make-person "lizhi" "feng"))
(define person3 (make-person "xing" "miao"))
(define person4 (make-person "chong" "yang"))

(define (print-person person) (person 'print))

(for-each print-person
	  (reverse ((ordering string<?
			      exact-person-firstName)
		    (list person1 person2 person3 person4))))
