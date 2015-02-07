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

;Care for the interpration principle of the key word and 
(define (make-joint account old-pass new-pass)
  (and (number? ((account old-pass 'withdraw) 0))
       (lambda (pass msg)
	 (if (eq? pass new-pass)
	     (account old-pass msg)
	     (account 'bad-pass 'foo)))))

(define acc (make-acount 100 'secret-pasword))

(define bcc (make-joint acc 'secret-password 'secret-password2))

((bcc 'secret-password2 'withdraw) 10)

((acc 'secret-password 'withdraw) 40)
