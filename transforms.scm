(cond-expand
 (guile
  (define-module (transforms)
    #:use-module (base)
    #:use-module (expander)
    #:export (syntactic-environment))
  
  (define-syntax define-transform
    (syntax-rules ()
      ((define-transform name . transforms)
       (define-syntax name
	 (lambda (stx)
	   (let* ((expression (syntax->datum stx))
		  (expanded (expand expression
				    'transforms)))
	     (datum->syntax stx expanded)))))))

  (define-transform define-core-transform
    (('define-core-transform name . transforms)
     ('set! 'syntactic-environment
	    ('append ('quote transforms)
		    'syntactic-environment))))
  )
 (jsssssssss
  (include "expander.scm")

  (define-transform define-core-transform  
    (('define-core-transform name (pattern template) ...)
     ('begin
       ('set! 'syntactic-environment
	      ('append ('quote ((pattern template) ...))
		       'syntactic-environment))
       ('define-transform name
	 (pattern template) ...))))
  
  ))

(define syntactic-environment '())

(define-core-transform and
  (('and)
   #true)
  
  (('and last)
   last)

  (('and first . rest)
   ('if first ('and . rest) #false))
  )

(define-core-transform or
  (('or)
   #false)

  (('or last)
   last)

  (('or first . rest)
   ('let ((result first))
     ('if result result ('or . rest))))
  )

(define-core-transform let*
  #;(('let* () expression)
   expression)

  (('let* () . body)
   ('let () . body))

  (('let* ((name-1 value-1)
  	   (name-2 value-2) ...)
     . body)
   ('let ((name-1 value-1))
     ('let* ((name-2 value-2) ...)
       . body)))
  )

(define-core-transform let
  (('let ((name value) ...)
     . body)
   (('lambda (name ...) . body) value ...))
  )

(define-core-transform define
  (('define (name . args) . body)
   ('define name ('lambda args . body)))
  )

(define-core-transform cond
  (('cond ('else result1 result2 ...))
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
        ('cond clause1 clause2 ...)))
  )

(define-core-transform when
  (('when test . actions)
   ('if test ('begin . actions)))
  )

(define-core-transform unless
  (('unless test . actions)
   ('when ('not test) . actions))
  )

(define-core-transform quasiquote
  (('quasiquote ('unquote form))
   form)

  (('quasiquote (('unquote-splicing form) . rest)) 
   ('append form ('quasiquote rest)))

  (('quasiquote ('quasiquote form) . depth) 
   ('list ('quote 'quasiquote)
	  ('quasiquote form nest . depth)))

  (('quasiquote ('unquote form) x . depth) 
   ('list ('quote 'unquote)
	  ('quasiquote form . depth)))

  (('quasiquote ('unquote-splicing form) nest . depth) 
   ('list ('quote 'unquote-splicing)
	  ('quasiquote form . depth)))

  (('quasiquote (car . cdr) . depth) 
   ('cons ('quasiquote car . depth)
	  ('quasiquote cdr . depth)))

  (('quasiquote atom . depth) 
   ('quote atom))
  )

(define-core-transform parameterize
  (('parameterize ((parameter value) ...) . body)
   ('begin
     ('push-parameter parameter value)
     ...
     ('let ((result ('begin . body)))
       ('pop-parameter parameter)
       ...
       result)))
  )

(define-core-transform is
  (('is '_ < b)
   ('lambda (a)
     ('is a < b)))

  (('is a < '_)
   ('lambda (b)
     ('is a < b)))

  (('is a < b)
   (< a b))
  )

(define-core-transform isnt
  (('isnt '_ ok?)
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
  )

(define-core-transform for
  (('for variable 'in list . actions)
   ('for-each ('lambda (variable) . actions)
	     list)))

(define-core-transform match
  (('match (combination . args) . patterns)
   ('let ((value (combination . args)))
     ('match value . patterns)))

  (('match value (pattern . actions) . rest)
   ('let ((fail ('lambda () ('match value . rest))))
     ('match-clause value pattern ('begin . actions)
		    (fail))))

  (('match value)
   ('error ''no-matching-pattern value))
  )

(define-core-transform match-clause
  (('match-clause value '_ sk fk)
   sk)
  
  (('match-clause value ('quote literal) sk fk)
   ('if ('equal? value ('quote literal))
	sk
	fk))
  
  (('match-clause value ('quasiquote
			 ('unquote identifier)) sk fk)
   ('let ((identifier value)) sk))
  
  (('match-clause value ('quasiquote (head . tail)) sk fk)
   ('if ('pair? value)
	('let ((first ('car value))
	       (rest ('cdr value)))
	  ('match-clause
	   first ('quasiquote head)
	   ('match-clause rest ('quasiquote tail) sk fk)
	   fk))
	fk))

  (('match-clause value ('quasiquote literal) sk fk)
   ('if ('equal? value ('quasiquote literal))
	sk
	fk))
  
  (('match-clause value (compound . pattern) sk fk)
   ('error ''compound-patterns-not-supported
	   '(compound . pattern)))

  (('match-clause value atom sk fk)
   ('if ('symbol? ('quote atom))
	('let ((atom value))
	  sk)
	('if ('equal? atom value)
	     sk
	     fk)))
  )

(define-core-transform e.g.
  (('e.g. expression '===> value)
   ('let ((result expression)
	  (source ('quote expression))
	  (expectation ('quote value)))
     ('if ('equal? result expectation)
	  (('valid-example) source result expectation)
	  (('invalid-example) source result expectation))))

  (('e.g. expression)
   ('let ((result expression)
	  (source ('quote expression)))
     ('if result
	  (('valid-example) source result)
	  (('invalid-example) source result)))))

((lambda (a) ((lambda (b) ((lambda (result~0) (if result~0 result~0 (+ a b))) (> a b))) (* a 2))) 5)


(e.g.
 (parameterize ((unique-symbol-counter 0))
   (expand '(with-transform ((('is a < b)
			      (< a b)))
			    (let* ((a 5)
				   (b (* a 2)))
			      (or (is a > b)
				  (+ a b))))
	   syntactic-environment))
 ===>
 ((lambda (a)
    ((lambda (b)
       ((lambda ()
	  ((lambda (result~0)
	     (if result~0
		 result~0
		 (+ a b)))
	   (> a b)))))
     (* a 2)))
  5))
