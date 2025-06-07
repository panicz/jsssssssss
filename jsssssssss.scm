#!/usr/bin/guile \
-L . -s
!#
(cond-expand
 (guile
  (import (ice-9 string-fun))
  (import (base))
  (import (expander))
  (import (transforms))
  (import (read)))
 (jsssssssss
  (include "transforms.scm")))

(define (symbol->js expression)
  (fold-left (lambda (string substitution)
	       (apply string-replace-substring
		      string
		      substitution))
	     (let ((initial (symbol->string expression)))
	       (cond
		((char-numeric? (string-ref initial 0))
		 (string-append "$N" initial))
		((or (string=? initial "for")
		     )
		 (string-append "$Kw" initial))
		(else
		 initial)))
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
	       ("." "$Dt")
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
;	       ("\0" "\\0")
           ;;; TODO... (\[abfve]? perhaps unicode stuff \u__ too?)
	       )))


(define (js-representation lisp-data)
  (cond
   ((null? lisp-data)
    "__nil")
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
    (string-append "{car: " (js-representation (car lisp-data))
                   ",cdr: " (js-representation (cdr lisp-data)) "}"))
   ((char? lisp-data)
    (string-append 
     "{char: \""
     (match lisp-data
       ('#\" "\\\"")
       ('#\\ "\\\\")
       ('#\x0a "\\n") ;; !!
       (c (let ((n (char->integer c)))
	    (if (is n < 32)
		(string-append
		 "\\x"(if (is n < 16)
			  "0"
			  "")
		 (number->string n 16))
		(list->string (list c))))))
     "\"}"))
   ((string? lisp-data)
    (string-append "\"" (string-escape lisp-data) "\""))
   ((vector? lisp-data)
    (string-append
     "["
     (string-join (map js-representation (vector->list lisp-data)) ",")
     "]"))))

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

(define (unlist-dot-arg args)
  (define (symtail as)
    (if (pair? as) (symtail (cdr as)) as))
  (let ((sym (symbol->js (symtail args))))
    (string-append "var " sym "=vector$Mn$Gtlist(" sym ");\n"))) ;; XD

(define (to-js expression)
  (match expression
    
    (`(lambda ,args . ,body)
     (string-append
      "(("(args-to-js args)")=>{\n"
      (if (list? args) "" (unlist-dot-arg args)) ;; he_he
      (sequence-to-js body) "})"))

    (`(if ,condition
	  ,consequent
	  ,alternative)
     (string-append
      "(("(to-js condition)")===false"
      "?("(to-js alternative)")"
      ":("(to-js consequent)"))"))

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
     (string-append
      "(()=>{\nwhile("(to-js condition)"){\n"
      (to-js `(begin . ,actions))"\n}})()"))
    
    (`(catch ,handler ,expression)
     (string-append
      "(()=>{\ntry{\nreturn "(to-js expression)"\n}"
      "catch(__e){\nreturn "(to-js handler)"(__e);"
      "\n};})()"))
    
    (`(try-finally ,attempt ,eventually)
     (string-append
      "(()=>{try{\nreturn "(to-js attempt)"\n}"
      "finally{\n"(to-js eventually)"\n};})()"))

    (`(,fun . ,args)
     (if (list? args)
	 (string-append
	  (to-js fun)
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
     (string-append "return " (to-js exp) ";\n"))
    (`(,exp . ,seq*)
     (string-append (to-js exp) ";\n"
		    (sequence-to-js seq*)))))

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
