#!/usr/bin/guile \
-L . -s
!#
(import (ice-9 string-fun))
(import (ice-9 textual-ports))
(import (base))
(import (expander))
(import (transforms))

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
    (string-append
     "{car: "(js-representation (car lisp-data))
     ", cdr: "(js-representation (cdr lisp-data))"}"))
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
     (string-append (to-js function) "("(string-join
					 (map to-js args)",")")"))

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
  (display (call-with-input-file file get-string-all)))

(rewrite "runtime/primops.js")
(rewrite "runtime/lists.js")
(rewrite "runtime/strings.js")
(rewrite "runtime/vectors.js")
(rewrite "runtime/parameters.js")
(rewrite "runtime/ports.js")
(rewrite "runtime/serialize.js")

(let* ((program (read-all))
       (expressions (expand-program program convenience-transforms)))
  (for expression in expressions
    (display "// ")
    (write expression)
    (newline)
    (display (to-js expression))
    (display ";")
    (newline)))
