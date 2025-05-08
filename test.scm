(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(console.log (factorial 5))

(console.log (apply + 1 2 '(3 4 5)))
(console.log (apply + '(1 2 3)))

(define (counter n)
  (lambda ()
    (set! n (+ n 1))
    n))

(define c (counter 0))

(console.log (c))
(console.log (c))
(console.log (c))

(define p (make-parameter 0))

(console.log (p))

(parameterize ((p 10))
  (console.log (p))
  (p (+ (p) 1))
  (console.log (p)))

(console.log (p))

(console.log (call-with-output-string
	       (lambda (p)
		 (write-char #\d p)
		 (write-char #\u p)
		 (write-char #\p p)
		 (write-char #\a p))))

(console.log (with-output-to-string
	       (lambda ()
		 (write-char #\d)
		 (write-char #\u)
		 (write-char #\p)
		 (write-char #\a))))

(console.log
 (call-with-input-string "dupa"
   (lambda (input)
     (list->string
      (let* ((d (read-char input))
	     (u (read-char input))
	     (p (read-char input))
	     (a (read-char input)))
	(list d u p a))))))

(console.log
 (with-input-from-string "dupa"
   (lambda ()
     (list->string
      (let* ((d (read-char))
	     (u (read-char))
	     (p (read-char))
	     (a (read-char)))
	(list d u p a))))))

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

(console.log (string-join '("Mary" "marry" "merry") ", "))

(when (is 2 > 3)
  (error "dupa"))

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
