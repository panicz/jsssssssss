(define-module (base)
  #:export-syntax (e.g.
		   assert
		   match
		   for
		   is
		   isnt)
  #:export (fold-left
	    every
	    any
	    only
	    take drop
	    fix
	    in
	    union
	    difference
	    set=?
	    transpose
	    read-all))

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

(define-syntax match-clause
  (syntax-rules (quote quasiquote unquote _)
    ((match-clause value _ sk fk)
     sk)
    
    ((match-clause value 'literal sk fk)
     (if (equal? value 'literal)
	 sk
	 fk))
    
    ((match-clause value `,identifier sk fk)
     (let ((identifier value)) sk))
    
    ((match-clause value `(head . tail) sk fk)
     (if (pair? value)
	 (let ((first (car value))
	       (rest (cdr value)))
	   (match-clause first `head
			 (match-clause rest `tail sk fk)
			 fk))
	 fk))

    ((match-clause value `literal sk fk)
     (if (equal? value `literal)
	 sk
	 fk))
    
    ((match-clause value (compound . pattern) sk fk)
     (syntax-error 'compound-patterns-not-supported '(compound . pattern)))

    ((match-clause value atom sk fk)
     (if (symbol? 'atom)
	 (let ((atom value))
	   sk)
	 (if (equal? atom value)
	     sk
	     fk)))
    ))
    
(define-syntax match
  (syntax-rules (quote quasiquote unquote _)
    ((match (combination . args) . patterns)
     (let ((value (combination . args)))
       (match value . patterns)))

    ((match value (pattern . actions) . rest)
     (let ((fail (lambda () (match value . rest))))
       (match-clause value pattern (begin . actions) (fail))))

    ((match value)
     (error 'no-matching-pattern value))
    ))

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

(define-syntax for
  (syntax-rules (in)
    ((for variable in list
       . actions)
     (for-each (lambda (variable)
		 . actions)
	       list))))

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

(define only filter)

(define (difference a b)
  (only (isnt _ in b) a))

(e.g.
 (difference '(a b c) '(b)) ===> (a c))

(define (set=? s0 . s*)
  (define (set=? a b)
    (or (equal? a b)
	(and (every (is _ in b) a)
	     (every (is _ in a) b))))
  (every (is _ set=? s0) s*))

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

(define* (read-all #:optional (port (current-input-port)))
  (let ((input (read port)))
    (if (eof-object? input)
	'()
	(let* ((result `(,input)))
	  (let next ((tip result))
	    (let ((input (read port)))
	      (cond
	       ((eof-object? input)
		result)
	       (else
		(set-cdr! tip `(,input))
		(next (cdr tip))))))))))

(e.g.
 (call-with-input-string "1 (2 3) 4" read-all)
 ===> (1 (2 3) 4))
