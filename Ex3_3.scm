(define (make-acount balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance  (+ balance amount)))
  (define (dispatch input-passwd m)
    (if (eq? input-passwd password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknow request -- MAKE-ACCOUNT" m)))
	(lambda (amount)
	  "Incorrect password")))
  dispatch)

(define acc (make-acount 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
