(define-module (expander)
  #:use-module (base)
  #:export (expand
	    expand-program
	    core-transforms))

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

(define and-transform
  '((('and)
     #true)
    
    (('and last)
     last)

    (('and first . rest)
     ('if first ('and . rest) #false))
    ))

(define or-transform
  '((('or)
     #false)

    (('or last)
     last)

    (('or first . rest)
     ('let ((result first))
       ('if result result ('or . rest))))
    ))

(define let*-transform
  '((('let* () expression)
     expression)

    (('let* () . body)
     ('let () . body))

    (('let* ((name-1 value-1)
  	     (name-2 value-2) ...)
       . body)
     ('let ((name-1 value-1))
       ('let* ((name-2 value-2) ...)
	 . body)))
    ))

(define let-transform
  '((('let ((name value) ...)
       . body)
     (('lambda (name ...) . body) value ...))
    ))

(define define-transform
  '((('define (name . args) . body)
     ('define name ('lambda args . body)))
    ))

(define single-begin-lambda-transform
  '((('lambda args ('begin . body))
     ('lambda args . body))
    ))

(define core-transforms
  (append
   single-begin-lambda-transform
   and-transform
   or-transform
   define-transform
   let-transform
   let*-transform))

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
    (assert (apply equal? names))
    (match names
      (`(,names . ,_)
       (apply map list names values))
      ('()
       '()))))

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

(e.g.
 (prefix-length even? '(2 4 6 7 8 9)) ===> 3)

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
  (let* ((unzipped (filter (lambda (key+value)
			     (is (car key+value) member keys))
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

(define (expand-program program transforms)
  ;; NOTE: because `define-transform` and `with-transform`
  ;; cons their patterns and templates in front of the `transforms`
  ;; list, they need to be used in reverse order, i.e. 
  ;; the most general matches should be written first,
  ;; and the most specific ones - last, as in, say:
  ;;
  ;;   (define-transform ('or first . rest)
  ;;      ('let ((result first))
  ;;        ('if result result ('or . last))))
  ;;
  ;;   (define-transform ('or last) last)
  ;;
  ;;   (define-transform ('or) #false)
  ;;
  (match program
    ('() '())
    
    (`((define-transform ,pattern ,template) . ,rest)
     (expand-program rest `((,pattern ,template) . ,transforms)))

    (`(,expression . ,expressions)
     `(,(expand expression transforms) . ,(expand-program expressions transforms)))))

(define (expand expression transforms)
  
  (define (transform expression)
    (let ((result (any (lambda (pattern&template)
			 (let ((bindings (bind (car pattern&template)
					       expression '())))
			   (and bindings
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

    (`(if ,condition ,then ,else)
     `(if ,(expand condition transforms)
	  ,(expand then transforms)
	  ,(expand else transforms)))

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
    
    (`(with-transform () . ,body)
     (let ((expanded (expand-program body transforms)))
       (match expanded
	 (`(,single)
	  single)
	 (_
	  `(begin . ,expanded)))))
    
    (`(with-transform ((,pattern ,template) . ,etc)
		      . ,body)
     (expand `(with-transform ,etc . ,body)
	     `((,pattern ,template) . ,transforms)))
    
    (`(,operator . ,operands)
     (let ((transformed (fix transform expression)))
       (if (equal? expression transformed)
	   `(,(expand operator transforms)
	     . ,(map (lambda (operand)
		       (expand operand transforms))
		     operands))
       ;else
           (expand transformed transforms))))
    (_
     expression)))

(e.g.
 (parameterize ((unique-symbol-counter 0))
   (expand '(with-transform ((('is a < b)
			      (< a b)))
			    (let* ((a 5)
				   (b (* a 2)))
			      (or (is a > b)
				  (+ a b))))
	   core-transforms))
 ===> ((lambda (a)
	 ((lambda (b)
	    ((lambda (result~0)
	       (if result~0
		   result~0
		   (+ a b)))
	     (> a b)))
	  (* a 2)))
       5))

(define is-transform
  '((('is '_ < b)
     ('lambda (a)
       ('is a < b)))

    (('is a < '_)
     ('lambda (b)
       ('is a < b)))

    (('is a < b)
     (< a b))
    ))

(define isnt-transform
  '((('isnt '_ ok?)
     ('lambda (a)
       ('not (ok? a))))

    (('isnt a ok?)
     ('not (ok? a)))

    (('isnt '_ < b)
     ('lambda (a)
       ('not ('is a < b))))

    (('isnt a < '_)
     ('lambda (b)
       ('not ('is a < b))))

    (('isnt a < b)
     ('not ('is a < b)))
    ))

(define match-transform
  '((('match (combination . args) . patterns)
     ('let ((value (combination . args)))
       ('match value . patterns)))

    (('match value (pattern . actions) . rest)
     ('let ((fail ('lambda () ('match value . rest))))
       ('match-clause value pattern ('begin . actions) (fail))))

    (('match value)
     ('error ''no-matching-pattern value))

    (('match-clause value '_ sk fk)
     sk)
    
    (('match-clause value ('quote literal) sk fk)
     ('if ('equal? value ('quote literal))
	  sk
	  fk))
    
    (('match-clause value ('quasiquote ('unquote identifier)) sk fk)
     ('let ((identifier value)) sk))
    
    (('match-clause value ('quasiquote (head . tail)) sk fk)
     ('if ('pair? value)
	  ('let ((first ('car value))
		 (rest ('cdr value)))
	    ('match-clause first ('quasiquote head)
			   ('match-clause rest ('quasiquote tail) sk fk)
			   fk))
	  fk))

    (('match-clause value ('quasiquote literal) sk fk)
     ('if ('equal? value ('quasiquote literal))
	  sk
	  fk))
    
    (('match-clause value (compound . pattern) sk fk)
     ('error ''compound-patterns-not-supported '(compound . pattern)))

    (('match-clause value atom sk fk)
     ('if ('symbol? ('quote atom))
	  ('let ((atom value))
	    sk)
	  ('if ('equal? atom value)
	       sk
	       fk)))
    ))

(e.g.
 (parameterize ((unique-symbol-counter 0))
   (expand
    '(match '(+ 2 3)
     (`(+ ,a ,b)
      (+ a b))
     (_
      'fail))
    match-transform))
 ===> (let ((value~0 (quote (+ 2 3))))
	(let ((fail~1 (lambda ()
			(let ((fail~2 (lambda ()
					(error (quote no-matching-pattern)
					       value~0))))
			  (quote fail)))))
	  (if (pair? value~0)
	      (let ((first~3 (car value~0))
		    (rest~4 (cdr value~0)))
		(if (equal? first~3 (quasiquote +))
		    (if (pair? rest~4)
			(let ((first~5 (car rest~4))
			      (rest~6 (cdr rest~4)))
			  (let ((a first~5))
			    (if (pair? rest~6)
				(let ((first~7 (car rest~6))
				      (rest~8 (cdr rest~6)))
				  (let ((b first~7))
				    (if (equal? rest~8 (quasiquote ()))
					(+ a b)
					(fail~1))))
				(fail~1))))
			(fail~1))
		    (fail~1)))
	      (fail~1)))))

(define convenience-transforms
  (append
   core-transforms
   match-transform
   is-transform
   isnt-transform))
