;; evaln内核
(define (evaln exp env)
  (cond ((self-evaluating? exp) exp)
	((variables? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((and? exp) (eval-and (and-clauses exp) env))
	((or? exp) (eval-or (or-clauses exp) env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (evaln (cond-if exp) env))
	((let? expr) (evaln (let->combination expr) env))
	((application? exp)
	 (applyn (evaln (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;; applyn内核
(define (applyn procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error "Unknown procedure type -- APPLY" procedure))))

;; 求一组表达式的值
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (evaln (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; eval-if
(define (eval-if exp env)
  (if (true? (evaln (if-predicate exp) env))
      (evaln (if-consequent exp) env)
      (evaln (if-alternative exp) env)))

;; 按顺序运行一序列表达式
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (evaln (first-exp exps) env))
	(else (evaln (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

;; 变量赋值
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (evaln (assignment-value exp) env)
		       env)
  'ok)

;; 变量定义
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
		    (evaln (definition-value exp) env)
		    env)
  'ok)

;; 语法分析
;; 数字和字符串自求值
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))

(define (variables? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
		   (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (caddr exp)
      #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (extend-cond-syntax? clause) (eq? (cadr clause) '=>))

(define (extend-cond-test clause) (car clause))

(define (extend-cond-recipient clause) (caddr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(cond ((cond-else-clause? first)
	       (if (null? rest)
		   (sequence->exp (cond-actions first))
		   (error "ELSE clause isn't last -- COND-IF"
			  clauses)))
	      ((extend-cond-syntax? first)
	       (make-if (extend-cond-test first)
			(list (extend-cond-recipient) (extend-cond-test first))
			(expand-clauses rest)))
	      (else 
	       (make-if (cond-predicate first)
			(sequence->exp (cond-actions first))
			(expand-clauses rest)))))))

(define (let? expr) (tagged-list? expr 'let))

(define (let-vars expr) (map car (cadr expr)))

(define (let-inits expr) (map cadr (cadr expr)))

(define (let-body expr) (cddr expr))

(define (let*? expr) (tagged-list? expr 'let*))

(define (let*-body expr) (caddr expr))

(define (let*-inits expr) (cadr expr))

(define (let*->nested-lets expr)
  (let ((inits (let*-inits expr))
	(body (let*-body expr)))
    (define (make-lets exprs)
      (if (null? exprs)
	  body
	  (list 'let (list (car exprs)) (make-lets (cdr exprs)))))
    (make-lets inits)))

(define (and? expr) (tagged-list? expr 'and))

(define (and-clauses expr) (cdr expr))

(define (eval-and exprs env)
  (let ((v (evaln (first-exp exprs) env)))
    (cond ((last-exp? exprs)
	   (if v v #f))
	  (else (if v
		    (eval-and (rest-exps exprs) env)
		    #f)))))

(define (or? expr) (tagged-list? expr 'or))

(define (or-clauses expr) (cdr expr))

(define (eval-or exprs env)
  (let ((v (evaln (first-exp exprs) env)))
    (cond ((last-exp? exprs))
	  (else (if v
		    v
		    (eval-or (rest-exps exprs) env))))))

(define (named-let? expr) (and (let? expr) (symbol? (cadr expr))))

(define (named-let-func-name expr) (cadr expr))

(define (named-let-func-body expr) (cadddr expr))

(define (named-let-func-parameters expr) (map car (caddr expr)))

(define (named-let-func-inits expr) (map cadr (caddr expr)))

(define (named-let->func expr)
  (list 'define
	(cons (named-let-func-name expr) (named-let-func-parameters))
	(named-let-func-body expr)))

(define (let->combination expr)
  (if (named-let? expr)
      (sequence->exp
       (list (named-let->func expr)
	     (cons (named-let-func-name) (named-let-func-inits expr))))
      (cons (make-lambda (let-vars expr)
			 (list (let-body expr)))
	    (let-inits expr))))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-value frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variables" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-value frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound varible -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-value frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-value frame))))

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! '#t #t initial-env)
    (define-variable! '#f #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '= =)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (applyn
   (primitive-implementation proc) args))

(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")

(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (evaln input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(driver-loop) 



