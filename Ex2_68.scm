(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-1 current-branch bits)
    (if (leaf? current-branch)
	bits
	(let ((l-branch (left-branch current-branch))
	      (r-branch (right-branch current-branch)))
	  (cond ((contains (symbols l-branch) symbol)
		 (encode-symbol-1 l-branch
				  (append bits (list 0))))
		((contains (symbols r-branch) symbol)
		 (encode-symbol-1 r-branch
				  (append bits (list 1))))
		(else (error "Bad Char --ENCODE-SYMBOL" symbol))))))
  (if (leaf? tree)
      (if (eq? symbol (symbol-leaf tree))
	  (encode-symbol-1 tree (list 0))
	  (error "Bad Char --ENCODE-SYMBOL" symbol))
      (encode-symbol-1 tree '())))

(define (contains set symbol)
  (cond ((null? set) #f)
	((eq? (car set) symbol) #t)
	(else (contains (cdr set) symbol))))

(define tree
  (make-code-tree
   (make-code-tree (make-leaf 'E 1)
		   (make-leaf 'F 1))
   (make-code-tree (make-leaf 'G 1)
		   (make-leaf 'H 1))))

(encode '(E F G H) tree)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge branches)
  (cond ((null? branches) '())
	((= 1 (length branches))
	 (car branches))
	(else (successive-merge (adjoin-set (make-code-tree (car branches)
							    (cadr branches))
					    (cddr branches))))))
;;Ex2_70
(define rock-pairs
  '((a 2) (na 16) (boom 1) (Sha 3) (Get 2) (yip 9) (job 2) (Wah 1)))

(define rock-song '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))

(encode rock-song (generate-huffman-tree rock-pairs))

(decode '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1
 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1
 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1) (generate-huffman-tree rock-pairs))

;Ex2_71 n-1 1
					
;Ex2_72 O(n^2)

