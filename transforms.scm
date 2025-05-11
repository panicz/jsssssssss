(define-module (transforms)
  #:use-module (base)
  #:use-module (expander)
  #:export (core-transforms
	    convenience-transforms))

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

(define cond-transform
  '((('cond ('else result1 result2 ...))
     ('begin result1 result2 ...))

    (('cond (test '=> result))
     ('let ((temp test))
       ('if temp (result temp))))

    (('cond (test '=> result) clause1 clause2 ...)
     ('let ((temp test))
       ('if temp
           (result temp)
           ('cond clause1 clause2 ...))))

    (('cond (test))
     test)

    (('cond (test) clause1 clause2 ...)
     ('let ((temp test))
       ('if temp
           temp
           ('cond clause1 clause2 ...))))

    (('cond (test result1 result2 ...))
     ('if test ('begin result1 result2 ...)))

    (('cond (test result1 result2 ...)
            clause1 clause2 ...)
     ('if test
          ('begin result1 result2 ...)
          ('cond clause1 clause2 ...)))))

(define when-unless-transform
  '((('when test . actions)
     ('if test ('begin . actions)))

    (('unless test . actions)
     ('when ('not test) . actions))))

(define quasiquote-transform 
  '((('quasiquote ('unquote form))
     form)

    (('quasiquote (('unquote-splicing form) . rest)) 
     ('append form ('quasiquote rest)))

    (('quasiquote ('quasiquote form) . depth) 
     ('list ('quote 'quasiquote) ('quasiquote form nest . depth)))

    (('quasiquote ('unquote form) x . depth) 
     ('list ('quote 'unquote) ('quasiquote form . depth)))

    (('quasiquote ('unquote-splicing form) nest . depth) 
     ('list ('quote 'unquote-splicing) ('quasiquote form . depth)))

    (('quasiquote (car . cdr) . depth) 
     ('cons ('quasiquote car . depth) ('quasiquote cdr . depth)))

    (('quasiquote atom . depth) 
     ('quote atom))))

(define parameterize-transform
  '((('parameterize ((parameter value) ...) . body)
     ('begin
       ('push-parameter parameter value)
       ...
       ('let ((result ('begin . body)))
	 ('pop-parameter parameter)
	 ...
	 result)))))

(define core-transforms
  (append
   and-transform
   or-transform
   define-transform
   let-transform
   let*-transform
   quasiquote-transform
   parameterize-transform
   cond-transform
   when-unless-transform
   ))

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

(define example-transforms
  '((('e.g. expression '===> value)
     ('let ((result expression))
       ('unless ('equal? result ('quote value))
	 (('invalid-example) ('quote expression) ('quote value) result))
       result))

    (('e.g. expression)
     ('let ((result expression))
       ('unless result
	 (('invalid-example) ('quote expression)))
       result))))

(define convenience-transforms
  (append
   core-transforms
   match-transform
   example-transforms
   is-transform
   isnt-transform))

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


