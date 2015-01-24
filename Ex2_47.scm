(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin f)
  (car f)))

(define (edge1 f)
  (cadr f))

(define (edge2 f)
  (cddr f))

(edge2 (make-frame 1 2 3))
