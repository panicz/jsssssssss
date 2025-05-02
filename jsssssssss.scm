#!/usr/bin/guile \
-L . -s
!#
(import (ice-9 match))
(import (ice-9 string-fun))
(import (ice-9 textual-ports))
(import (base))
(import (expander))

(define preamble (call-with-input-file "preamble.js" get-string-all))

(define (symbol->js expression)
  (string-append "s__"
                 (fold-left (lambda (string substitution)
	                          (apply string-replace-substring
		                             string
		                             substitution))
	                        (symbol->string expression)
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
				  ))))

(define (string-escape string)
  (fold-left (lambda (string substitution)
	       (apply string-replace-substring
		      string
		      substitution))
	     string
	     '(("\\" "\\\\")
	       ("\"" "\\\"")
	       )))

(define (js-representation lisp-data)
  (cond
   ((list? lisp-data)
    (string-append "[" (string-join (map js-representation lisp-data) ",") "]"))
   ((symbol? lisp-data)
    (string-append "{symbol: \""(symbol->js lisp-data)"\"}"))
   ((number? lisp-data)
    (number->string lisp-data))
   ((eq? lisp-data #t)
    "true")
   ((eq? lisp-data #f)
    "false")
   ((pair? lisp-data)
    (string-append
     "{car: "(js-representation (car lisp-data))
     ", cdr: "(js-representation (cdr lisp-data))"}"))
   ((string? lisp-data)
    (string-append "\"" (string-escape lisp-data) "\""))))

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

    (`(lambda ,args (begin . ,body))
     (to-js `(lambda ,args . ,body))) ;; :o~~~

    (`(lambda ,args ,body)
     (string-append
      "(("(args-to-js args)")=>" (to-js body) ")"))

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
      "?(unspecified)"
      ":("(to-js then)"))"))

    (`(begin . ,operations)
     (string-append "{" (string-join (map to-js operations) ";") "}"))
     
    (`(set! ,variable ,expression)
     (string-append
      (symbol->js variable)"="(to-js expression)))

    (`(define ,variable ,expression)
     (string-append
      "var "(symbol->js variable)" = "(to-js expression)))

    (`(quote ,literal)
     (js-representation literal))

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

(display preamble)

(let* ((program (read-all))
       (expressions (expand-program program core-transforms)))
  (for expression in expressions
    (display (to-js expression))
    (display ";")
    (newline)))
