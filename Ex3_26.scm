(define (make-record key value) 
   (list (cons key value) nil nil)) 
(define (get-key record) (caar record)) 
(define (get-value record) (cdar record)) 
(define (set-key! record new-key) (set-car! (car record) new-key)) 
(define (set-value! record new-value) (set-cdr! (car record) new-value)) 
(define (get-left record) (cadr record)) 
(define (get-right record) (caddr record)) 
(define (set-left! record new-left) (set-car! (cdr record) new-left)) 
(define (set-right! record new-right) (set-car! (cddr record) new-right)) 
  
(define (assoc key records) 
  (cond ((null? records) false) 
        ((equal? key (get-key records)) (get-value records)) 
        ((< key (get-key records)) (assoc key (get-left records))) 
        (else (assoc key (get-right records))))) 
 
(define (add-record key value table) 
  (define (iter record parent set-action) 
    (cond ((null? record) (let ((new (make-record key value))) 
                            (set-action parent new) 
                            (car new))) 
          ((equal? key (get-key record)) (set-value! record value) 
                                         (car record)) 
          ((< key (get-key record)) (iter (get-left record) record set-left!)) 
          (else (iter (get-right record) record set-right!)))) 
  (iter (cdr table) table set-cdr!)) 
  
; the procedure 
 
(define (make-table) 
 
  (let ((local-table (list '*table*))) 
 
    (define (lookup keys) 
      (define (iter keys records) 
        (if (null? keys) records 
          (let ((found (assoc (car keys) records))) 
            (if found (iter (cdr keys) found) 
              false)))) 
      (iter keys (cdr local-table))) 
 
    (define (insert! keys value) 
      (define (iter keys subtable) 
        (cond ((null? (cdr keys)) (add-record (car keys) value subtable)) 
              (else (let ((new (add-record (car keys) nil subtable))) 
                      (iter (cdr keys) new))))) 
      (iter keys local-table) 
      'ok) 
 
    (define (print) (display local-table) (newline)) 
 
    (define (dispatch m) 
      (cond ((eq? m 'lookup-proc) lookup) 
            ((eq? m 'insert-proc!) insert!) 
            ((eq? m 'print) print) 
            (error "Unknown operation - TABLE" m))) 
    dispatch)) 
 
(define operation-table (make-table)) 
(define get (operation-table 'lookup-proc)) 
(define put (operation-table 'insert-proc!)) 
(define print-table (operation-table 'print))
