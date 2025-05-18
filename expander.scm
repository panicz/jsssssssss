(cond-expand
 (guile
  (define-module (expander)
    #:use-module (base)
    #:use-module (read)
    #:export (expand
	      expand-program
	      unique-symbol-counter)))
  (jsssssssss
   (include "read.scm")))
   
;; to find out what actual transforms look like (to be passed
;; as the second argument to the `expand` and `expand-program`
;; procedures, check out the `transforms.scm` file

(define (fix f argument)
  (let ((value (f argument)))
    (if (equal? value argument)
        value
    ;else
        (fix f value))))

(define (transpose list-of-lists)
  (if (null? list-of-lists)
      '()
  ;else
      (apply map list list-of-lists)))

(e.g.
 (transpose '((1 2 3)
	      (4 5 6))) ===> ((1 4)
			      (2 5)
			      (3 6)))

(define (used-symbols expression)
  (match expression
    (`(quote ,literal)
     '())
    (`(,repeated ... . ,rest)
     (union (used-symbols repeated)
	    (used-symbols rest)))
    (`(,head . ,tail)
     (union (used-symbols head)
	    (used-symbols tail)))
    (_ 
     (if (symbol? expression)
	 `(,expression)
    ;else
	 '()))))

(define unique-symbol-counter
  (make-parameter 0))

(define (unique-symbol base-symbol)
  (let ((ordinal (unique-symbol-counter)))
    (unique-symbol-counter (+ ordinal 1))
    (string->symbol
     (string-append
      (symbol->string base-symbol)
      "~"(number->string ordinal)))))

(define (bind pattern #;to form
	      #;given bound-variables)
  (match pattern
    (`(quote ,literal)
     (and (equal? form literal)
          bound-variables))
    (`(,repetition ... . ,remaining)
     (bind-sequence repetition remaining form
                    bound-variables))
    (`(,head/pattern . ,tail/pattern)
     (match form
       (`(,head/form . ,tail/form)
        (let ((bound (bind head/pattern head/form
                           bound-variables)))
          (and bound
               (bind tail/pattern tail/form bound))))
       (_
        #false)))
    (_
     (if (symbol? pattern)
         (merge-bindings `((,pattern . ,form))
			 bound-variables)
     ;else
         (and (equal? pattern form)
              bound-variables)))))

(define (merge-bindings bindings . bindings*)
  (define (merge-bindings a b)
    (and a b
         (fold-left
	  (lambda (bindings key+value)
	    (and bindings
                 (cond
		  ((assoc (car key+value) bindings)
                   => (lambda (key+value*)
			(and (equal?
			      (cdr key+value*)
			      (cdr key+value))
			     bindings)))
		  (else
		   `(,key+value
		     . ,bindings)))))
          a
          b)))
  (fold-left merge-bindings bindings bindings*))

(define (zip-bindings list-of-bindings)
  (let ((names (map (lambda (bindings)
		      (map car bindings))
		    list-of-bindings))
	(values (map (lambda (bindings)
		       (map cdr bindings))
		     list-of-bindings)))
    ;;(assert (apply equal? names))
    (match names
      (`(,names . ,_)
       (apply map list names values))
      ('()
       '()))))

(writeln "dupa")

(e.g.
 (zip-bindings '(((a . 1) (b . 2) (c . 3))
                 ((a . 4) (b . 5) (c . 6))
                 ((a . 7) (b . 8) (c . 9))))
 ===> ((a 1 4 7) (b 2 5 8) (c 3 6 9)))

(define (prefix-length condition? l)
  (define (traverse l n)
    (match l
      (`(,head . ,tail)
       (if (condition? head)
  	 (traverse tail (+ n 1))
       ;else
         n))
      (_
       n)))
  (traverse l 0))

;;(e.g. (prefix-length even? '(2 4 6 7 8 9)) ===> 3)

(define (carry #;from prefix #;to suffix #;until success?)
  (let ((result (success? prefix suffix)))
    (if (or result (null? prefix))
        result
    ;else
	(let* ((n (length prefix))
	       (initial (take (- n 1) prefix))
	       (last (car (drop (- n 1) prefix))))
	  (carry #;from initial #;to `(,last . ,suffix)
                        #;until success?)))))

(define (bind-sequence repeated-pattern remaining-pattern
                       form bound-variables)
  (define (successful-match? prefix suffix)
    (let* ((bindings (map (lambda (form)
			    (bind repeated-pattern form '()))
                          prefix))
           (zipped (if (null? prefix)
		       (map list (used-symbols repeated-pattern))
		       (zip-bindings bindings)))
           (merged (merge-bindings bound-variables zipped)))
      (and merged (bind remaining-pattern suffix merged))))
  
  (let* ((limit (prefix-length (lambda (constituent)
                                 (bind repeated-pattern
                                       constituent '()))
                               form))
	 (prefix (take limit form))
	 (rest (drop limit form)))
    (carry #;from prefix #;to rest
		  #;until successful-match?)))

(e.g.
 (bind '('let* ((name-1 value-1)
  		(name-2 value-2) ...)
	  . body)
       '(let* ((a 5)
	      (b (* a 2)))
	 (or (is a > b)
	     (+ a b)))
       '())
 ===> ((value-1 . 5)
       (name-1 . a)
       (name-2 b)
       (value-2 (* a 2))
       (body (or (is a > b) (+ a b)))))

(define (fill template #;with bindings)
  (let* ((missing (difference (used-symbols template)
			      (map (lambda (key+value)
				     (car key+value))
				   bindings)))
	 (bindings `(,@(map (lambda (symbol)
			      `(,symbol . ,(unique-symbol
					    symbol)))
			    missing) ,@bindings)))
    (fill-template template bindings)))

(define (fill-template template #;with bindings)
  (match template
    (`(quote ,literal)
     literal)
    (`(,repeated ... . ,rest)
     `(,@(fill-sequence repeated bindings)
       . ,(fill-template rest #;with bindings)))
    (`(,head . ,tail)
     `(,(fill-template head #;with bindings)
       . ,(fill-template tail #;with bindings)))
    (_
     (cond ((and (symbol? template)
                 (assoc template bindings))
            => (lambda (key+value)
                 (cdr key+value)))
           (else
            template)))))

(define (unzip-bindings bindings keys)
  (let* ((unzipped (only (lambda (key+value)
			     (member (car key+value) keys))
			   bindings))
	 (names (map car unzipped))
	 (values (map cdr unzipped)))
    (map (lambda (singular-values)
	   `(,@(map (lambda (name value)
		      `(,name . ,value))
		    names singular-values)
	     ,@bindings))
	 (transpose values))))

(e.g.
 (unzip-bindings '((a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4)) '(a c e))
 ===> (((a . 1) (c . 1) (a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4))
       ((a . 2) (c . 2) (a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4))
       ((a . 3) (c . 3) (a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4))))

(define (fill-sequence template bindings)
  (let* ((symbols (used-symbols template))
	 (binding-sequences (unzip-bindings bindings symbols)))
    (map (lambda (bindings)
	   (fill-template template bindings))
	 binding-sequences)))

(define cond-expand-features '(jsssssssss))

(define (cond-expand-features-match? condition)
  (match condition
    (`(or . ,subconditions)
     (any cond-expand-features-match? subconditions))
    (`(and . ,subconditions)
     (every cond-expand-features-match? subconditions))
    (`(not ,feature)
     (not (cond-expand-features-match? feature)))
    (_
     (member condition cond-expand-features))))

(define (first-matching cond-expand-clauses)
  (match cond-expand-clauses
    (`((else . ,fragment))
     fragment)
    (`((,condition . ,fragment) . ,rest)
     (if (cond-expand-features-match? condition)
	 fragment
	 (first-matching rest)))))

(define (expand-program program transforms)
  (with-output-to-port (current-error-port)
    (lambda ()
      (writeln "expanding "program)))
  (match program
    ('() '())
    
    (`((define-transform ,name . ,patterns+templates)
       . ,rest)
     (expand-program rest `(,@patterns+templates
			    ,@transforms)))

    (`((begin . ,subprogram))
     (expand-program subprogram transforms))

    (`((begin . ,subprogram) . ,rest)
     (expand-program `(,@subprogram . ,rest) transforms))

    (`((include ,path) . ,rest)
     (let ((content (call-with-input-file path
		      (lambda (from-port)
			(read-upto +inf.0 from-port)))))
       (expand-program `(,@content . ,rest) transforms)))

    (`((cond-expand . ,clauses) . ,rest)
     (expand-program `(,@(first-matching clauses) . ,rest)
		     transforms))
    
    (`(,expression . ,expressions)
     (let ((expanded (expand expression transforms)))
       (if (equal? expanded expression)
           `(,expression . ,(expand-program expressions
                                            transforms))
           (expand-program `(,expanded . ,expressions)
                           transforms))))))

(define (expand expression transforms)
  
  (define (transform expression)
    (let ((result (any (lambda (pattern&template)
			 (let ((bindings (bind (car pattern&template)
					       expression '())))
			   (and bindings
				(with-output-to-port
				    (current-error-port)
				  (lambda ()
				    (writeln "matched "
					     pattern&template)
				    #t))

				
				`(,bindings
				  ,(cadr pattern&template)))))
		       transforms)))
      (match result
	(`(,bindings ,template)
	 (fill template bindings))

	(_
	 expression))))

  (match expression
    (`(quote ,_)
     expression)

    (`(lambda ,args . ,body)
     `(lambda ,args . ,(expand-program body transforms)))

    (`(if ,condition ,consequent ,alternative)
     `(if ,(expand condition transforms)
	  ,(expand consequent transforms)
	  ,(expand alternative transforms)))

    (`(if ,condition ,then)
     `(if ,(expand condition transforms)
	  ,(expand then transforms)))
    
    (`(begin . ,expressions)
     (let ((expanded (expand-program expressions transforms)))
       (match expanded
	 (`(,single)
	  single)
	 (_
	  `(begin . ,expanded)))))
    
    (`(set! ,variable ,value)
     `(set! ,(expand variable transforms)
	    ,(expand value transforms)))
    (`(with-transform ,patterns+templates . ,body)
     (let ((expanded (expand-program body `(,@patterns+templates
					    ,@transforms))))
       (match expanded
	 (`(,single)
	  single)
	 (_
	  `(begin . ,expanded)))))
    
    (`(,operator . ,operands)
     (let ((transformed (fix transform expression)))
       (if (equal? expression transformed)
	   `(,(expand operator transforms)
	     . ,(if (list? operands)
		    (map (lambda (operand)
			   (expand operand transforms))
			 operands)
		    operands))
       ;else
           (expand transformed transforms))))
    (_
     expression)))
