#!/usr/bin/guile \
-L . -s
!#
(import (ice-9 match))
(import (ice-9 string-fun))
(import (ice-9 textual-ports))
(import (expander))

;; JavaScript Scrounged from a Simple and Straightforward Subset of Scheme

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
  (let ((expression (expand expression core-macros)))
    (match expression
      (`(lambda ,args . ,body)
       (string-append
	"(("(args-to-js args)")=>("(string-join
				    (map to-js body)",")"))"))

      (`(if ,test ,then ,else)
       (string-append
	"(("(to-js test)")===false"
	"?("(to-js else)")"
	":("(to-js then)"))"))

      (`(set! ,variable ,expression)
       (string-append
	(symbol->js variable)"="(to-js expression)))

      (`(define ,variable ,expression)
       (string-append
	"var "(symbol->js variable)" = "(to-js expression)";"))

      (`(quote ,literal)
       (js-representation literal))

      (`(,function . ,args)
       (string-append (to-js function) "("(string-join
					   (map to-js args)",")")"))

      (_
       (if (symbol? expression)
	   (symbol->js expression)
	   (js-representation expression)))
      )))

(display preamble)

(let loop ()
  (let ((expression (read)))
    (unless (eof-object? expression)
      (display (to-js expression))
      (display ";")
      (newline)
      (loop))))
