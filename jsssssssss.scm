(import (ice-9 match))
(import (ice-9 string-fun))

;; JavaScript Scrounged from a Simple and Straightforward Subset of Scheme

(define preamble "
function cons(a,b) { 
  if (Array.isArray(b)) {
    return [a].concat(b); 
  }
  return {car: a, cdr: b};
}

function car(a) {
  if (Array.isArray(a)) {
    return a[0];
  }
  return a.car;
}

function cdr(a) { 
  if (Array.isArray(a)) {
    return a.slice(1);
  }
  return a.cdr;
}

function eq$Qu(...args) {
  var eq = (a,b) => a === b || (Array.isArray(a) && Array.isArray(b) && !a.length && !b.length)
  for (var i = 1; i < args.length; ++i) {
    if (!eq(args[i-1], args[i])) {
      return false;
    }
  }
  return true;
}

function eqv$Qu(...args) {
  var eqv = (a,b) => a == b || (Array.isArray(a) && Array.isArray(b) && !a.length && !b.length)
  for (var i = 1; i < args.length; ++i) {
    if (!eqv(args[i-1], args[i])) {
      return false;
    }
  }
  return true;
}

const $Eq = eq$Qu;

function $Pl(...args) {
  var result = 0; 
  for (var x of args) {
    result += x;
  }
  return result;
}

function $Mn(...args) {
  if (args.length <= 1) {
    return -args[0];
  }
  var result = args[0];
  for (var i = 1; i < args.length; ++i) {
    result -= args[i];
  }
  return result;
}

function $St(...args) {
  var result = 1; 
  for (var x of args) {
    result *= x;
  }
  return result;
}

function $Sl(...args) {
  if (args.length <= 1) {
    return 1/(args[0]);
  }
  var result = args[0];
  for (var i = 1; i < args.length; ++i) {
    result /= args[i];
  }
  return result;
}

function not(x) { return !x; }

function $Ls(...args) {
  for (var i = 1; i < args.length; ++i) {
    if (!(args[i-1] < args[i])) {
      return false;
    }
  }
  return true;
}

function $Gt(...args) {
  for (var i = 1; i < args.length; ++i) {
    if (!(args[i-1] > args[i])) {
      return false;
    }
  }
  return true;
}

function $Ls$Eq(...args) {
  for (var i = 1; i < args.length; ++i) {
    if (!(args[i-1] <= args[i])) {
      return false;
    }
  }
  return true;
}

function $Gt$Eq(...args) {
  for (var i = 1; i < args.length; ++i) {
    if (!(args[i-1] >= args[i])) {
      return false;
    }
  }
  return true;
}

function apply(f, ...args) {
  var collected = args.slice(0, args.length-1).concat(args[args.length-1]);
  return f.apply(null, collected);
}

")

(define (fold-left f init l)
  (match l
    ('()
     init)
    (`(,h . ,t)
     (fold-left f (f init h) t))))


(define (symbol->js expression)
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
	       )))

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
    (`(lambda ,args . ,body)
     (string-append
      "(("(args-to-js args)")=>("(string-join
				  (map to-js body)",")"))"))
    
    (`(if ,test ,then ,else)
     (string-append
      "(("(to-js test)")"
      "?("(to-js then)")"
      ":("(to-js else)"))"))
    
    (`(set! ,variable ,expression)
     (string-append
      (symbol->js variable)"="(to-js expression)))

    (`(define (,function . ,args) . ,body)
     (string-append
      "function "(to-js function)"("(args-to-js args)"){"
      "return "(string-join (map to-js body)",")";"
      "}"))

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
    ))

(display preamble)

(let loop ()
  (let ((expression (read)))
    (unless (eof-object? expression)
      (display (to-js expression))
      (display ";")
      (newline)
      (loop))))
