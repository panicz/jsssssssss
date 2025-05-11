;; oto program, ktory chcemy potraktowac
;; naszym konwerterem

(import (base))

(define (read-upto max-items from-port)
  (let ((result '())
	(total-items 0))

    (define (terminating? c)
      (or (eof-object? c) (eq? c #\))))

    (define (add-item! item)
      (set! result (append! result `(,item)))
      (set! total-items (+ total-items 1)))

    (define (add-improper! item)
      (set! result (append! result item))
      (set! total-items max-items))
    
    (define (skip-spaces)
      (while (char-whitespace? (peek-char from-port))
	(read-char from-port)))

    (define (end-of-line? c)
      (or (eq? c #\x0a)
	  (eof-object? c)))
    
    (define (skip-line-comment)
      (while (isnt (read-char from-port) end-of-line?)))

    (define (skip-block-comment)
      (let ((inside-comment? #t))
	(while inside-comment?
	  (let ((c (read-char from-port)))
	    (when (eof-object? c)
	      (set! inside-comment? #f))
	    (when (eq? c #\|)
	      (let ((d (peek-char from-port)))
		(when (eq? d #\#)
		  (read-char from-port)
		  (set! inside-comment? #f))))
	    (when (eq? c #\#)
	      (let ((d (peek-char from-port)))
		(when (eq? d #\|)
		  (read-char from-port)
		  (skip-block-comment))))))))
    
    (define (separator? c)
      (or (eof-object? c)
	  (eq? c #\()
	  (eq? c #\))
	  (eq? c #\;)
	  (eq? c #\x0a) ;;#\newline
	  (eq? c #\x20) ;;#\space
	  (eq? c #\x09) ;;#\tab
	  (eq? c #\")))
    
    (define (read-atom . prefix)
      (call-with-output-string
	(lambda (to-string)
	  (for-each (lambda (c) (write-char c to-string))
		    prefix)
	  (while (isnt (peek-char from-port) separator?)
	    (write-char (read-char from-port) to-string)))))

    (define (parse-atom . prefix)
      (let ((atom (apply read-atom prefix)))
	(cond
	 ;; tutaj bysmy chcieli sprawdzic, czy mamy;
	 ;; - liczbe calkowita /^[+-]?[0-9]+$/
	 ;; - liczbe zmiennopozycyjna /^[+-]?[0-9]+[.][0-9]*$/
	 ;;   albo /^[+-]?[0-9]*[.][0-9]+$/
	 ;;   albo /^[+-]?[0-9]+([.][0-9]*)?e[+-][0-9]+$/
	 ;; - liczbe wymierna /^[0-9]+[/][0-9]+$/
	 ;; - liczbe zespolona:
	 ;;    /^[0-9]+([.][0-9])?[+-][0-9]+([.][0-9]*)?i$/
         ;;    itd.
	 (else
	  (string->symbol atom)))))

    (define (read-string)
      (call-with-output-string
	(lambda (to-string)
	  (let ((inside-string? #t))
	    (while inside-string?
	      (match (read-char from-port)
		('#\"
		 (set! inside-string? #f))
		('#\\
		 (match (read-string)
		   ('#\" (write-char #\' to-string))
		   ('#\\ (write-char #\\ to-string))
		   ('#\n (write-char #\x0a to-string))
		   ('#\t (write-char #\x09 to-string))))
		(c
		 (write-char c to-string))))))))

    (while (is total-items < max-items)
      (skip-spaces)
      (let ((c (read-char from-port)))
	(match c
	  ('#\.
	   (add-improper! (read-upto 1 from-port))
	   (skip-spaces)
	   (let ((d (read-char from-port)))
	     (assert (terminating? d))))
	   
	  ('#\(
	   (add-item! (read-upto +inf.0 from-port)))

	  ('#\;
	   (skip-line-comment))
	  
	  ('#\"
	   (add-item! (read-string)))

	  ('#\'
	   (let ((item (read-upto 1 from-port)))
	     (add-item! `(quote . ,item))))
	  
	  ('#\`
	   (let ((item (read-upto 1 from-port)))
	     (add-item! `(quasiquote . ,item))))
	  
	  ('#\,
	   (let* ((q (if (eq? (peek-char from-port) #\@)
			       (begin
				 (read-char from-port)
				 'unquote-splicing)
			       'unquote))
		  (item (read-upto 1 from-port)))
	     (add-item! `(,q . ,item))))
	  ('#\#
	   (let ((d (read-char from-port)))
	     (match d
	       ('#\(
		(let ((content (read-upto +inf.0 from-port)))
		  (list->vector content)))
	       ('#\\
		(let ((char-name (read-atom)))
		  (cond
		   ((= (string-length char-name) 1)
		    (string-ref char-name 0))
		   ((eq? (string-ref char-name 0) #\x)
		    (integer->char
		     (string->number (string-drop char-name 1) 16)))
		   (else
		    (error "named characters not supported yet")))))
	       ('#\|
		(skip-block-comment))
	       ('#\;
		(read-upto 1 from-port)) ;and ignore it
	       ('#\t
		(let ((atom (read-atom d)))
		  (assert (or (equal? atom "t")
			      (equal? atom "true")))
		  (add-item! #t)))
	       ('#\f
		(let ((atom (read-atom d)))
		  (assert (or (equal? atom "f")
			      (equal? atom "false")))
		  (add-item! #f)))
	       ('#\'
		(let ((item (read-upto 1 from-port)))
		  (add-item! `(syntax . ,item))))
	       
	       ('#\`
		(let ((item (read-upto 1 from-port)))
		  (add-item! `(quasisyntax . ,item))))
	       
	       ('#\,
		(let* ((s (if (eq? (peek-char from-port) #\@)
				     (begin
				       (read-char from-port)
				       'unsyntax-splicing)
				     'unsyntax))
		       (item (read-upto 1 from-port)))
		  (add-item! `(,s . ,item))))
	       ('#\x
		(let ((hexadecimal (read-atom)))
		  (string->number hexadecimal 16)))
	       ('#\o
		(let ((octal (read-atom)))
		  (string->number octal 8)))
	       ('#\b
		(let ((binary (read-atom)))
		  (string->number binary 2)))
	       (_
		(error "Unsupported hash extension: "(read-atom d)))
	       )))
	  (_
	   (if (terminating? c)
	       (set! total-items max-items)
	       (add-item! (parse-atom c)))))))
    result))

(call-with-input-string " (+ 1 2 (* a b)) "
  (lambda (from-string)
    (read-upto 1 from-string)))
 
