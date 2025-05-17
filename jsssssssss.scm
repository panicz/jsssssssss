#!/usr/bin/guile \
-L . -s
!#
(import (ice-9 string-fun))
(import (base))
(import (expander))
(import (transforms))
(import (read))

(define (symbol->js expression)
  (fold-left (lambda (string substitution)
	       (apply string-replace-substring
		      string
		      substitution))
	     (let ((initial (symbol->string expression)))
	       (if (char-numeric? (string-ref initial 0))
		   (string-append "$N" initial)
		   initial))
	     '(("+" "$Pl")
	       ("-" "$Mn")
	       ("*" "$St")
	       ("/" "$Sl")
	       ("<" "$Ls")
	       (">" "$Gt")
	       ("=" "$Eq")
	       ("!" "$Ex")
	       ("%" "$Pc")
	       ("?" "$Qu")
	       ("@" "$At")
	       ("~" "$Tl")
	       ("&" "$Am")
	       ("#" "$Nm")
	       )))

(define (string-escape string)
  (fold-left (lambda (string substitution)
	       (apply string-replace-substring
		      string
		      substitution))
	     string
	     '(("\\" "\\\\")
	       ("\"" "\\\"")
	       ("\n" "\\n")
	       ("\r" "\\r")
	       ("\t" "\\t")
	       ("\0" "\\0")
           ;;; TODO... (\[abfve]? perhaps unicode stuff \u__ too?)
	       )))

(define (improper-list-representation c)
  (with-output-to-string
    (lambda ()
      (write-string "{improper: [")
      (while (pair? c)
	(write-string (js-representation (car c)))
	(if (pair? (cdr c))
	    (write-string ",")
	    (write-string "]"))
	(set! c (cdr c)))
      (write-string ", tail: ")
      (write-string (js-representation c))
      (write-string "}"))))

(define (js-representation lisp-data)
  (cond
   ((list? lisp-data)
    (string-append "[" (string-join (map js-representation lisp-data) ",") "]"))
   ((symbol? lisp-data)
    (string-append "{symbol: \""(symbol->js lisp-data)"\"}"))
   ((number? lisp-data)
    (cond ((finite? lisp-data)
	   (number->string lisp-data))
	  ((is lisp-data > 0)
	   "Infinity")
	  ((is lisp-data < 0)
	   "-Infinity")
	  (else
	   (assert (nan? lisp-data))
	   "NaN")))
   ((eq? lisp-data #t)
    "true")
   ((eq? lisp-data #f)
    "false")
   ((pair? lisp-data)
    (improper-list-representation lisp-data))
   ((char? lisp-data)
    (string-append 
     "{char: '"
     (match lisp-data
       ('#\' "\\'")
       ('#\\ "\\\\")
       ('#\newline "\\n")
       (c (let ((n (char->integer c)))
	    (if (is n < 32)
		(string-append
		 "\\x"(if (is n < 16)
			  "0"
			  "")
		 (number->string n 16))
		(list->string (list c))))))
     "'}"))
   ((string? lisp-data)
    (string-append "\"" (string-escape lisp-data) "\""))
   ((vector? lisp-data)
    (string-append
     "{vector: ["
     (string-join (map js-representation (vector->list lisp-data)) ",")
     "]}"))))

(define (args-to-js args)
  (match args
    (`(,last)
     (symbol->js last))
    (`(,first . ,rest)
     (string-append
      (symbol->js first)","(args-to-js rest)))
    ('()
     "")
    (last
     (string-append "..."(symbol->js last)))))

(define (to-js expression)
  (match expression

    (`(lambda ,args . ,body)
     (string-append
      "(("(args-to-js args)")=>{" (sequence-to-js body) "})"))

    (`(if ,test ,then ,else)
     (string-append
      "(("(to-js test)")===false"
      "?("(to-js else)")"
      ":("(to-js then)"))"))

    (`(if ,test ,then)
     (string-append
      "(("(to-js test)")===false"
      "?undefined"
      ":("(to-js then)"))"))

    (`(begin)
     (to-js '(if #f #f)))
    
    (`(begin . ,operations)
     (to-js `((lambda () . ,operations))))
     
    (`(set! ,variable ,expression)
     (string-append
      (symbol->js variable)"="(to-js expression)))

    (`(define ,variable ,expression)
     (string-append
      "var "(symbol->js variable)" = "(to-js expression)))

    (`(quote ,literal)
     (js-representation literal))

    (`(while ,condition . ,actions)
     (string-append "(()=>{while("(to-js condition)"){"
		    (to-js `(begin . ,actions))"}})()"))
    
    (`(catch ,handler ,expression)
     (string-append "(()=>{try{return "(to-js expression)"}"
		    "catch(__e){return "(to-js handler)"(__e);};})()"))
    
    (`(try-finally ,try ,finally)
     (string-append "(()=>{try{return "(to-js try)"}"
		    "finally{"(to-js finally)"};})()"))
    
    (`(,function . ,args)
     (if (list? args)
	 (string-append
	  (to-js function)
	  "("(string-join
	      (map to-js args)",")")")
	 (js-representation expression)))

    (_
     (if (symbol? expression)
	 (symbol->js expression)
	 (js-representation expression)))
    ))

(define (sequence-to-js seq)
  (match seq
    (`(,exp)
     (string-append "return " (to-js exp) ";"))
    (`(,exp . ,seq*)
     (string-append (to-js exp) ";" (sequence-to-js seq*)))))

(define (rewrite file)
  (with-input-from-file file
    (lambda ()
      (while (isnt (peek-char) eof-object?)
	(write-char (read-char))))))

(rewrite "runtime/primops.js")
(rewrite "runtime/lists.js")
(rewrite "runtime/strings.js")
(rewrite "runtime/numbers.js")
(rewrite "runtime/vectors.js")
(rewrite "runtime/parameters.js")
(rewrite "runtime/ports.js")
(rewrite "runtime/serialize.js")

(let* ((program (read-upto +inf.0 (current-input-port)))
       (expressions (expand-program
		     program
		     syntactic-environment)))
  (for expression in expressions
       #|
    (display "// ")
    (write expression)
       (newline)
       |#
    (display (to-js expression))
    (display ";")
    (newline)))
