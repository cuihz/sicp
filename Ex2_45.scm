(define (split orig-placer split-placer)
  (lambda (painter n)
    (cond ((= n 0) painter)
	  (else
	   (let ((smaller ((split orig-placer split-placer)
			   painter
			   (- n 1))))
	     (orig-placer painter (split-placer smaller smaller)))))))



