(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(e.g. (factorial 5) ===> 120)

(e.g. (apply + 1 2 '(3 4 5)) ===> 15)
(e.g. (apply + '(1 2 3)) ===> 6)

(define (counter n)
  (lambda ()
    (set! n (+ n 1))
    n))

(define c (counter 0))

(e.g. (c) ===> 1)
(e.g. (c) ===> 2)
(e.g. (c) ===> 3)

(define p (make-parameter 0))

(e.g. (p) ===> 0)

(parameterize ((p 10))
  (e.g. (p) ===> 10)
  (p (+ (p) 1))
  (e.g. (p) ===> 11))

(e.g. (p) ===> 0)

(e.g.
 (call-with-output-string
   (lambda (p)
     (write-char #\d p)
     (write-char #\u p)
     (write-char #\p p)
     (write-char #\a p))) ===> "dupa")

(e.g.
 (with-output-to-string
   (lambda ()
     (write-char #\d)
     (write-char #\u)
     (write-char #\p)
     (write-char #\a))) ===> "dupa")

(e.g.
 (call-with-input-string "dupa"
   (lambda (input)
     (list->string
      (let* ((d (read-char input))
	     (u (read-char input))
	     (p (read-char input))
	     (a (read-char input)))
	(list d u p a))))) ===> "dupa")

(e.g.
 (with-input-from-string "dupa"
   (lambda ()
     (list->string
      (let* ((d (read-char))
	     (u (read-char))
	     (p (read-char))
	     (a (read-char)))
	(list d u p a))))) ===> "dupa")

(writeln (current-output-port))
(writeln current-output-port)
(writeln "a'\"\\b")

(e.g.
 (catch (lambda (e) (slot-ref e 'message))
   (e.g. (+ 2 2) ===> 5)) ===> "while evaluating

  (+ 2 2)

expected:

  5

got:

  4
")

(e.g. (string->list "dupa") ===> (#\d #\u #\p #\a))

(e.g.
 (with-output-to-string
   (lambda ()
     (for-each write-char (string->list "dupa")))) ===> "dupa")
 

(e.g.
 (with-output-to-string
   (lambda ()
     (with-input-from-string "dupa"
       (lambda ()
	 (while (isnt (peek-char) eof-object?)
	   (write-char (read-char))))))) ===> "dupa")

(e.g. (/ 1 0) ===> +inf.0)
(e.g. (/ -1 0) ===> -inf.0)
(e.g. (/ 0 0) ===> +nan.0)
(e.g. (/ 1 2) ===> 0.5)
(e.g. (/ 2 2 2 2) ===> 0.25)

;; the following test only runs when invoking
;;
;;   $ ./jsssssssss.scm < test.scm > test.js
;;   $ node test.js
;;
;; and breaks when invoked as
;;
;;   $ ./jsssssssss.scm < test.scm | node
;;
;; (which isn't that bad after all)

(console.log "type any key and press enter:")
(let ((c (read-char)))
  (console.log (string-append "you typed: " (serialize c))))
