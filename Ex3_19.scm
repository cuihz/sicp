;One step Two step;
;They will meet

(define (contains-cycle? lst) 
   (define (safe-cdr l) 
     (if (pair? l) 
         (cdr l) 
         '())) 
   (define (iter a b) 
     (cond ((not (pair? a)) #f) 
           ((not (pair? b)) #f) 
           ((eq? a b) #t) 
           (else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))))) 
   (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))

(define x '(1 2 3 4 5 6 7 8))

(define y '(1 2 3 4 5 6 7 8))

(set-cdr! (cdddr (cddddr y)) (cdddr y))

(define z '(1))

(set-cdr! z z)

x
y
z

(contains-cycle? x)
(contains-cycle? y)
(contains-cycle? z)

