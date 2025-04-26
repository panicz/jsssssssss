(import (ice-9 match))

(define-syntax e.g.
  (syntax-rules (===>)
    ((e.g. expression ===> value)
     (let ((result expression))
       (unless (equal? result 'value)
	 (error "while evaluating "
		'expression
		"\nexpected:\n"
		'value
		"\ngot:\n"result))
       result))

    ((e.g. expression)
     (let ((result expression))
       (unless result
	 (error "expected "'expression
		"\nto be non-#false"))
       result))))

(e.g. (+ 2 2) ===> 4)

(define-syntax assert
  (syntax-rules ()
    ((assert condition)
     (let ((result condition))
       (unless result
	 (error "Assertion failed: "'condition))
       result))))

(assert (= (+ 2 2) 4))

(define (fold-left f init l)
  (match l
    ('()
     init)
    (`(,h . ,t)
     (fold-left f (f init h) t))))

(e.g.
 (fold-left (lambda (x y)
	      `(,x + ,y))
	    'e
	    '(a b c d))
 ===> ((((e + a) + b) + c) + d))

(define (every satisfying? elements)
  (match elements
    ('() #t)
    (`(,first . ,rest)
     (and (satisfying? first)
	  (every satisfying? rest)))))

(define (any satisfying? elements)
  (match elements
    ('() #f)
    (`(,first . ,rest)
     (or (satisfying? first)
	 (any satisfying? rest)))))

(define-syntax is
  (syntax-rules (_)
    ((is _ < b)
     (lambda (a)
       (is a < b)))
    ((is a < _)
     (lambda (b)
       (is a < b)))
    ((is a < b)
     (< a b))))

(e.g. (is 2 < 3))

(define-syntax isnt
  (syntax-rules (_)
    ((isnt _ ok?)
     (lambda (a)
       (not (ok? a))))
    ((isnt a ok?)
     (not (ok? a)))
    ((isnt _ < b)
     (lambda (a)
       (not (is a < b))))
    ((isnt a < _)
     (lambda (b)
       (not (is a < b))))
    ((isnt a < b)
     (not (is a < b)))))

(e.g. (isnt 3 < 2))

(define (fix function argument)
  (let ((value (function argument)))
    (if (equal? value argument)
        value
    ;else
        (fix function value))))

(define (take n elements)
  (if (is n <= 0)
      '()
      (match elements
	(`(,first . ,rest)
	 `(,first . ,(take (- n 1) rest)))
	(_
	 elements))))

(e.g.
 (take 3 '(a b c d e)) ===> (a b c))

(define (drop n elements)
  (if (is n <= 0)
      elements
      (match elements
	(`(,first . ,rest)
	 (drop (- n 1) rest))
	(_
	 elements))))

(e.g.
 (drop 3 '(a b c d e)) ===> (d e))

(define (in element set)
  (any (is _ equal? element) set))

(e.g.
 (is 'y in '(x y z)))

(define (union s0 . s*)
  (define (union a b)
    (fold-left (lambda (set element)
		 (if (is element in set)
		     set
		     `(,element . ,set)))
	       a b))
  (fold-left union s0 s*))
					
(e.g. (union '(a b c) '(c b n) '(a f d))
      ===> (d f n a b c))

(define (difference a b)
  (filter (isnt _ in b) a))

(e.g.
 (difference '(a b c) '(b)) ===> (a c))

(define (set=? s0 . s*)
  (define (set=? a b)
    (or (equal? a b)
	(and (every (is _ in b) a)
	     (every (is _ in a) b))))
  (every (is _ set=? s0) s*))

(define (used-symbols expression)
  (match expression
    (`(quote ,literal)
     '())
    (`(,repeated ,(? ...?) . ,rest)
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

(define (transpose list-of-lists)
  (if (null? list-of-lists)
      '()
  ;else
      (apply map list list-of-lists)))

(define core-macros
  '((('let ((name value) ...)
       . body)
     (('lambda (name ...) . body) value ...))
    (('let* () . body)
     ('begin . body))
    (('let* ((name-1 value-1)
  	     (name-2 value-2) ...)
       . body)
     ('let ((name-1 value-1))
       ('let* ((name-2 value-2) ...)
         . body)))
    (('and)
     #true)
    (('and last)
     last)
    (('and first . rest)
     ('if first ('and . rest) #false))
    (('or)
     #false)
    (('or last)
     last)
    (('or first . rest)
     ('let ((result first))
       ('if result result ('or . rest))))
    (('is '_ < b)
     ('lambda (a)
       ('is a < b)))

    (('is a < '_)
     ('lambda (b)
       ('is a < b)))

    (('is a < b)
     (< a b))

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
    
    ))

(define (...? s)
  (eq? s '...))

(define (bind pattern #;to form
	      #;given bound-variables)
  (match pattern
    (`(quote ,literal)
     (and (equal? form literal)
          bound-variables))
    (`(,repetition ,(? ...?) . ,remaining)
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
  (match list-of-bindings
    ((((names . values) ...) ...)
     (assert (apply equal? names))
     (match names
       (`(,names . ,_)
        (apply map list names values))
       ('()
        '())))))

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
	(match prefix
	  ((initial ... last)
	   (carry #;from initial #;to `(,last . ,suffix)
                        #;until success?))))))

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
    (`(,repeated ,(? ...?) . ,rest)
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

(define (expand expression macros)
  
  (define (transform expression)
    (let ((result (any (lambda (pattern&template)
			 (let ((bindings (bind (car pattern&template)
					       expression '())))
			   (and bindings
				`(,bindings
				  ,(cadr pattern&template)))))
		       macros)))
      (match result
	(`(,bindings ,template)
	 (fill template bindings))

	(_
	 expression))))
  
  (define (expand expression)
    (match expression
      (`(quote ,_)
       expression)
      (`(lambda ,args ,body)
       `(lambda ,args ,(expand body)))
      (`(if ,condition ,then ,else)
       `(if ,(expand condition)
	    ,(expand then)
	    ,(expand else)))
      (`(,operator . ,operands)
       (let ((transformed (fix transform expression)))
	 (if (equal? expression transformed)
	     `(,(expand operator) . ,(map expand operands))
         ;else
             (expand transformed))))
      (_
       expression)))

  (expand expression))

(e.g.
 (parameterize ((unique-symbol-counter 0))
   (expand '(let* ((a 5)
		   (b (* a 2)))
	      (or (is a > b)
		  (+ a b)))
	   core-macros))
 ===> ((lambda (a)
	 ((lambda (b)
	    (begin
	      ((lambda (result~0)
		 (if result~0 result~0 (+ a b)))
	       (> a b))))
	  (* a 2)))
       5))
